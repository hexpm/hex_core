-module(hex_tarball_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

memory_test() ->
    Metadata = #{
      <<"app">> => <<"foo">>,
      <<"version">> => <<"1.0.0">>,
      <<"maintainers">> => [<<"José">>],
      <<"build_tool">> => <<"rebar3">>
     },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Contents),
    {ok, #{checksum := Checksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory),
    ok.

disk_test() ->
    hex_test_helpers:in_tmp(fun() ->
        ok = file:write_file("foo.erl", <<"-module(foo).">>),
        ok = file:change_mode("foo.erl", 8#100644),
        ok = file:write_file("bad.erl", <<"bad">>),

        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>, <<"build_tool">> => <<"rebar3">>},
        Files = [".", "foo.erl"],
        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
        <<"53787F9D87A09DE9A31FB2F367E75CDE92605643A982E021000A2ECAC6384B21">> = hex_tarball:format_checksum(Checksum),
        {ok, #{checksum := Checksum, metadata := Metadata}} = hex_tarball:unpack(Tarball, "unpack"),
        {ok, ["foo.erl", "hex_metadata.config"]} = file:list_dir("unpack/"),
        {ok, <<"-module(foo).">>} = file:read_file("unpack/foo.erl"),
        {ok, <<"{<<\"app\">>,<<\"foo\">>}.\n{<<\"build_tool\">>,<<\"rebar3\">>}.\n{<<\"version\">>,<<\"1.0.0\">>}.\n">>} =
            file:read_file("unpack/hex_metadata.config")
    end).

timestamps_and_permissions_test() ->
    hex_test_helpers:in_tmp(fun() ->
        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

        ok = file:write_file("foo.sh", <<"">>),
        ok = file:change_mode("foo.sh", 8#100755),
        Files = [
            {"foo.erl", <<"">>},
            "foo.sh"
        ],

        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),

        %% inside tarball
        {ok, Files2} = hex_erl_tar:extract({binary, Tarball}, [memory]),
        {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, Files2),
        {ok, [FooErlEntry, FooShEntry]} = hex_erl_tar:table({binary, ContentsBinary}, [compressed, verbose]),
        Epoch = epoch(),

        {"foo.erl", regular, _, Epoch, 8#100644, 0, 0} = FooErlEntry,
        {"foo.sh", regular, _, Epoch, 8#100755, 0, 0} = FooShEntry,

        %% unpacked
        UnpackDir = "timestamps_and_permissions",
        {ok, #{checksum := Checksum}} = hex_tarball:unpack(Tarball, UnpackDir),

        {ok, FooErlFileInfo} = file:read_file_info(UnpackDir ++ "/foo.erl"),
        {ok, FooShFileInfo} = file:read_file_info(UnpackDir ++ "/foo.sh"),
        8#100644 = FooErlFileInfo#file_info.mode,
        8#100755 = FooShFileInfo#file_info.mode,
        [{{Year, _, _}, _}] = calendar:local_time_to_universal_time_dst(FooErlFileInfo#file_info.mtime),
        {{Year, _, _}, _} = calendar:local_time()
    end).

symlinks_test() ->
    hex_test_helpers:in_tmp(fun() ->
        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

        ok = file:make_dir("dir"),
        ok = file:write_file("dir/foo.sh", <<"foo">>),
        ok = file:make_symlink("dir/foo.sh", "dir/bar.sh"),

        Files = [
            "dir/foo.sh",
            "dir/bar.sh"
        ],

        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
        UnpackDir = "symlinks",
        {ok, #{checksum := Checksum}} = hex_tarball:unpack(Tarball, UnpackDir),
        {ok, _} = file:read_link_info(UnpackDir ++ "/dir/bar.sh"),
        ok
    end).

build_tools_test() ->
    Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Contents = [],

    {ok, {Tarball1, _}} = hex_tarball:create(maps:put(<<"files">>, [<<"Makefile">>], Metadata), Contents),
    {ok, #{metadata := #{<<"build_tools">> := [<<"make">>]}}} = hex_tarball:unpack(Tarball1, memory),

    {ok, {Tarball2, _}} = hex_tarball:create(maps:put(<<"build_tools">>, [<<"mix">>], Metadata), Contents),
    {ok, #{metadata := #{<<"build_tools">> := [<<"mix">>]}}} = hex_tarball:unpack(Tarball2, memory),

    {ok, {Tarball3, _}} = hex_tarball:create(Metadata, Contents),
    {ok, #{metadata := Metadata2}} = hex_tarball:unpack(Tarball3, memory),
    false = maps:is_key(<<"build_tools">>, Metadata2),

    ok.

requirements_test() ->
    ExpectedRequirements = #{
        <<"aaa">> => #{
            <<"app">> => <<"aaa">>,
            <<"optional">> => true,
            <<"requirement">> => <<"~> 1.0">>,
            <<"repository">> => <<"hexpm">>},
        <<"bbb">> => #{
            <<"app">> => <<"bbb">>,
            <<"optional">> => true,
            <<"requirement">> => <<"~> 1.0">>,
            <<"repository">> => <<"hexpm">>}},

    Normal = [{<<"aaa">>, [{<<"app">>, <<"aaa">>},
                           {<<"optional">>, true},
                           {<<"requirement">>, <<"~> 1.0">>},
                           {<<"repository">>, <<"hexpm">>}]},
              {<<"bbb">>, [{<<"app">>, <<"bbb">>},
                           {<<"optional">>, true},
                           {<<"requirement">>, <<"~> 1.0">>},
                           {<<"repository">>, <<"hexpm">>}]}],

    Legacy = [[{<<"name">>, <<"aaa">>},
               {<<"app">>, <<"aaa">>},
               {<<"optional">>, true},
               {<<"requirement">>, <<"~> 1.0">>},
               {<<"repository">>, <<"hexpm">>}],

              [{<<"name">>, <<"bbb">>},
               {<<"app">>, <<"bbb">>},
               {<<"optional">>, true},
               {<<"requirement">>, <<"~> 1.0">>},
               {<<"repository">>, <<"hexpm">>}]],

    ExpectedRequirements = hex_tarball:normalize_requirements(Normal),
    ExpectedRequirements = hex_tarball:normalize_requirements(Legacy),
    ok.

decode_metadata_test() ->
    #{<<"foo">> := <<"bar">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bar\">>}.">>),

    #{<<"foo">> := <<"bö/utf8">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö/utf8\">>}.">>),

    %% we should convert invalid latin1 encoded metadata to utf8 so that this becomes:
    %% #{<<"foo">> := <<"bö/utf8">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö\">>}.">>),
    #{<<"foo">> := <<"bö">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö\">>}.">>),

    {error, {metadata, invalid_terms}} = hex_tarball:do_decode_metadata(<<"ok[">>),

    {error, {metadata, {user, "illegal atom asdf"}}} = hex_tarball:do_decode_metadata(<<"asdf.">>),

    {error, {metadata, not_key_value}} = hex_tarball:do_decode_metadata(<<"ok.">>),

    ok.

unpack_error_handling_test() ->
    {ok, {Tarball, Checksum}} = hex_tarball:create(#{"name" => <<"foo">>}, [{"rebar.config", <<"">>}]),
    {ok, #{checksum := Checksum}} = hex_tarball:unpack(Tarball, memory),
    {ok, OuterFileList} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    OuterFiles = maps:from_list(OuterFileList),

    %% tarball

    {error, {tarball, eof}} = hex_tarball:unpack(<<"badtar">>, memory),

    {error, {tarball, empty}} = unpack_files(#{}),

    {error, {tarball, {missing_files, ["VERSION", "CHECKSUM"]}}} =
        unpack_files(#{"metadata.config" => <<"">>, "contents.tar.gz" => <<"">>}),

    {error, {tarball, {invalid_files, ["invalid.txt"]}}} =
        unpack_files(#{"invalid.txt" => <<"">>}),

    {error, {tarball, {bad_version, <<"1">>}}} =
        unpack_files(OuterFiles#{"VERSION" => <<"1">>}),

    {error, {tarball, invalid_checksum}} =
        unpack_files(OuterFiles#{"CHECKSUM" => <<"bad">>}),

    {error, {tarball, {checksum_mismatch, _, _}}} =
        unpack_files(OuterFiles#{"contents.tar.gz" => <<"">>}),

    %% metadata

    Files1 = OuterFiles#{
      "metadata.config" => <<"ok $">>,
      "CHECKSUM" => <<"1BB37F9A91F9E4A3667A4527930187ACF6B9714C0DE7EADD55DC31BE5CFDD98C">>
    },
    {error, {metadata, {illegal, "$"}}} = unpack_files(Files1),

    %% contents

    Files5 = OuterFiles#{
      "contents.tar.gz" => <<"badtar">>,
      "CHECKSUM" => <<"9AA6A026CCD93D4E7B3B6083595259D2946709DE8F9EC7FAB69F2E53939DF403">>
    },
    {error,{inner_tarball,eof}} = unpack_files(Files5),

    ok.

%%====================================================================
%% Helpers
%%====================================================================

epoch() ->
    NixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Y2kEpoch = calendar:datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}}),
    Y2kEpoch - NixEpoch.

unpack_files(Files) ->
    FileList = maps:to_list(Files),
    ok = hex_erl_tar:create("test.tar", FileList, [write]),
    {ok, Binary} = file:read_file("test.tar"),
    ok = file:delete("test.tar"),
    hex_tarball:unpack(Binary, memory).
