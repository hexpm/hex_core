-module(hex_tarball_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [disk_test, timestamps_and_permissions_test, symlinks_test,
     memory_test, meta_validation_test, build_tools_test, requirements_test,
     decode_metadata_test, unpack_error_handling_test,
     docs_test
    ].

memory_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"app">>  => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tools">> => [<<"rebar3">>],
        <<"files">> => [<<"rebar.config">>],
        <<"licenses">> => [<<"Apache">>],
        <<"description">> => <<"Simple, robust and performant Erlang web server">>,
        <<"requirements">> => #{}
    },

    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum, warnings := []}} = hex_tarball:create(Metadata, Contents),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory),
    ok.

disk_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    SrcDir = filename:join(BaseDir, "src"),
    EmptyDir = filename:join(BaseDir, "empty"),
    Foo = filename:join(SrcDir, "foo.erl"),
    UnpackDir = filename:join(BaseDir, "unpack"),

    ok = file:make_dir(SrcDir),
    ok = file:make_dir(EmptyDir),
    ok = file:change_mode(EmptyDir, 8#100755),
    ok = file:write_file(Foo, <<"-module(foo).">>),
    ok = file:change_mode(Foo, 8#100644),
    ok = file:write_file(filename:join(SrcDir, "not_whitelisted.erl"), <<"">>),

    Files = [{"empty", EmptyDir}, {"src", SrcDir}, {"src/foo.erl", Foo}],

    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"app">>  => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tools">> => [<<"rebar3">>],
        <<"files">> => [<<"rebar.config">>],
        <<"licenses">> => [<<"Apache">>],
        <<"description">> => <<"Simple, robust and performant Erlang web server">>,
        <<"requirements">> => #{}
    },

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum, warnings := []}} = hex_tarball:create(Metadata, Files),
    ?assertEqual(<<"8F9DB819DDD1BDFB246A200B84A374074265DA6FDCA329CD74E64A88420672A4">>,
                 hex_tarball:format_checksum(OuterChecksum)),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum, metadata := Metadata}} = hex_tarball:unpack(Tarball, UnpackDir),
    UnpackedFiles = [filename:join(UnpackDir, "empty"),
                     filename:join(UnpackDir, "hex_metadata.config"),
                     filename:join(UnpackDir, "src"),
                     filename:join([UnpackDir, "src", "foo.erl"])
                    ],
    ?assertMatch(UnpackedFiles, filelib:wildcard(filename:join(UnpackDir, "**/*"))),
    {ok, <<"-module(foo).">>} = file:read_file(filename:join(UnpackDir, "src/foo.erl")),
    {ok,<<"{<<\"app\">>,<<\"foo\">>}.\n{<<\"build_tools\">>,[<<\"rebar3\">>]}.\n{<<\"description\">>,<<\"Simple, robust and performant Erlang web server\">>}.\n{<<\"files\">>,[<<\"rebar.config\">>]}.\n{<<\"licenses\">>,[<<\"Apache\">>]}.\n{<<\"name\">>,<<\"foo\">>}.\n{<<\"requirements\">>,[]}.\n{<<\"version\">>,<<\"1.0.0\">>}.\n">>}
        =
        file:read_file(filename:join(UnpackDir, "hex_metadata.config")).


timestamps_and_permissions_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Foo = filename:join(BaseDir, "foo.sh"),

    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"app">>  => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tools">> => [<<"rebar3">>],
        <<"files">> => [<<"rebar.config">>],
        <<"licenses">> => [<<"Apache">>],
        <<"description">> => <<"Simple, robust and performant Erlang web server">>,
        <<"requirements">> => #{}
    },

    ok = file:write_file(Foo, <<"">>),
    ok = file:change_mode(Foo, 8#100755),
    Files = [
             {"foo.erl", <<"">>},
             {"foo.sh", Foo}
            ],

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(Metadata, Files),

    %% inside tarball
    {ok, Files2} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, Files2),
    {ok, [FooErlEntry, FooShEntry]} = hex_erl_tar:table({binary, ContentsBinary}, [compressed, verbose]),
    Epoch = epoch(),

    {"foo.erl", regular, _, Epoch, 8#100644, 0, 0} = FooErlEntry,
    {"foo.sh", regular, _, Epoch, 8#100755, 0, 0} = FooShEntry,

    %% unpacked
    UnpackDir = filename:join(BaseDir, "timestamps_and_permissions"),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:unpack(Tarball, UnpackDir),

    {ok, FooErlFileInfo} = file:read_file_info(UnpackDir ++ "/foo.erl"),
    {ok, FooShFileInfo} = file:read_file_info(UnpackDir ++ "/foo.sh"),
    8#100644 = FooErlFileInfo#file_info.mode,
    8#100755 = FooShFileInfo#file_info.mode,
    [{{Year, _, _}, _}] = calendar:local_time_to_universal_time_dst(FooErlFileInfo#file_info.mtime),
    {{Year, _, _}, _} = calendar:local_time().

symlinks_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"app">>  => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tools">> => [<<"rebar3">>],
        <<"files">> => [<<"rebar.config">>],
        <<"licenses">> => [<<"Apache">>],
        <<"description">> => <<"Simple, robust and performant Erlang web server">>,
        <<"requirements">> => #{}
    },

    Dir = filename:join(BaseDir, "dir"),
    FooSh = filename:join(Dir, "foo.sh"),
    BarSh = filename:join(Dir, "bar.sh"),
    ok = file:make_dir(Dir),
    ok = file:write_file(FooSh, <<"foo">>),
    ok = file:make_symlink(FooSh, BarSh),

    Files = [
             {"dir/foo.sh", FooSh},
             {"dir/bar.sh", BarSh}
            ],

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(Metadata, Files),
    UnpackDir = filename:join(BaseDir, "symlinks"),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:unpack(Tarball, UnpackDir),
    {ok, _} = file:read_link_info(filename:join([UnpackDir, "dir", "bar.sh"])),
    ok.

build_tools_test(_Config) ->

    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"app">>  => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"files">> => [],
        <<"licenses">> => [<<"Apache">>],
        <<"description">> => <<"Simple, robust and performant Erlang web server">>,
        <<"requirements">> => #{}
    },

    Contents = [],

    {ok, #{tarball := Tarball1}} = hex_tarball:create(maps:put(<<"files">>, [<<"Makefile">>], Metadata), Contents),
    {ok, #{metadata := #{<<"build_tools">> := [<<"make">>]}}} = hex_tarball:unpack(Tarball1, memory),

    {ok, #{tarball := Tarball2}} = hex_tarball:create(maps:put(<<"build_tools">>, [<<"mix">>], Metadata), Contents),
    {ok, #{metadata := #{<<"build_tools">> := [<<"mix">>]}}} = hex_tarball:unpack(Tarball2, memory),

    {ok, #{tarball := Tarball3}} = hex_tarball:create(Metadata, Contents),
    {ok, #{metadata := Metadata2}} = hex_tarball:unpack(Tarball3, memory),

    %% This check isn't valid anymore since buildtools is required metadata -- Bryan
    %% true = maps:is_key(<<"build_tools">>, Metadata2),
    [] = maps:get(<<"build_tools">>, Metadata2),

    ok.

requirements_test(_Config) ->
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

decode_metadata_test(_Config) ->
    #{<<"foo">> := <<"bar">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bar\">>}.">>),

    #{<<"foo">> := <<"bö/utf8">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö/utf8\">>}.">>),

    %% we should convert invalid latin1 encoded metadata to utf8 so that this becomes:
    %% #{<<"foo">> := <<"bö/utf8">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö\">>}.">>),
    #{<<"foo">> := <<"bö">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö\">>}.">>),

    {error, {metadata, invalid_terms}} = hex_tarball:do_decode_metadata(<<"ok[">>),

    {error, {metadata, {user, "illegal atom asdf"}}} = hex_tarball:do_decode_metadata(<<"asdf.">>),

    {error, {metadata, not_key_value}} = hex_tarball:do_decode_metadata(<<"ok.">>),

    ok.

meta_validation_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"ecto">>,
        <<"app">>  => <<"ecto">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tools">> => <<"mix">>,
        <<"files">> => [],
        <<"licenses">> => [<<"MIT">>],
        <<"description">> => <<"Ecto is not your ORM">>,
        <<"requirements">> => []
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{warnings := []}} = hex_tarball:create(Metadata, Contents),
    Metadata1 = maps:put(<<"contributors">>, [], Metadata),
    {ok, #{warnings := [{deprecated,<<"contributors">>}]}} = hex_tarball:create(Metadata1, Contents),
    Metadata2 = maps:put(<<"maintainers">>, [], Metadata1),
    {ok, #{warnings := [{deprecated, <<"contributors">>},{deprecated, <<"maintainers">>}]}} =
    hex_tarball:create(Metadata2, Contents),
    Metadata3 = maps:put(<<"maintainers">>, [], Metadata),
    {ok, #{warnings := [{deprecated, <<"maintainers">>}]}} = hex_tarball:create(Metadata3, Contents),
    Metadata4 = maps:put(<<"version">>, <<"1.0">>, Metadata),
    {error,{[{invalid,<<"version">>}],[]}} = hex_tarball:create(Metadata4, Contents),
    Metadata5 = maps:put(<<"unknown">>, <<"field">>, Metadata4),
    ExpErrs = {error, [{invalid, <<"version">>}],
                    [{unknown_field, <<"unknown">>}]},
    A = <<"the package version in the metadata is not a valid semantic version">>,
    B = <<"unknown is an unknown metadata field and will be ignored">>,
    A = hex_tarball:format_error(Metadata5, {invalid, <<"version">>}),
    B = hex_tarball:format_error(Metadata5, {unknown_field, <<"unknown">>}),
    [A,B] = hex_tarball:format_errors(Metadata5, ExpErrs),
    {ok, #{warnings :=  [{unknown_field,<<"unknown">>},
                      {invalid,<<"version">>}]}} = hex_tarball:create(Metadata5, Contents),
    ok.

unpack_error_handling_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"elli">>,
        <<"app">>  => <<"elli">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tools">> => [<<"rebar3">>],
        <<"files">> => [<<"rebar.config">>],
        <<"licenses">> => [<<"Apache">>],
        <<"description">> => <<"Simple, robust and performant Erlang web server">>,
        <<"requirements">> => #{}
    },
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(Metadata, [{"rebar.config", <<"">>}]),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:unpack(Tarball, memory),
    {ok, OuterFileList} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    OuterFiles = maps:from_list(OuterFileList),

    %% tarball

    {error, {tarball, eof}} = hex_tarball:unpack(<<"badtar">>, memory),

    {error, {tarball, empty}} = unpack_files(#{}),

    {error, {tarball, {missing_files, ["VERSION", "CHECKSUM"]}}} =
        unpack_files(#{"metadata.config" => <<"">>, "contents.tar.gz" => <<"">>}),

    {error, {tarball, {bad_version, <<"1">>}}} =
        unpack_files(OuterFiles#{"VERSION" => <<"1">>}),

    {error, {tarball, invalid_inner_checksum}} =
        unpack_files(OuterFiles#{"CHECKSUM" => <<"bad">>}),

    {error, {tarball, {inner_checksum_mismatch, _, _}}} =
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
      "CHECKSUM" => <<"63E0D44ED4F61F5A1636A516A6A26890052CE0BB1B1A6EDC66C30282E2EC1A58">>
    },

    {error, {tarball, {inner_checksum_mismatch, _, _}}} = unpack_files(Files5),

    %% I am not sure how to trigger this with the metadata changes. It may be an obsolete check -- Bryan
    %% {error,{inner_tarball,eof}} = unpack_files(Files5),

    ok.

docs_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    UnpackDir = filename:join(BaseDir, "unpack_docs"),

    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    {ok, Files} = hex_tarball:unpack_docs(Tarball, memory),

    ok = hex_tarball:unpack_docs(Tarball, UnpackDir),
    {ok, <<"Docs">>} = file:read_file(filename:join(UnpackDir, "index.html")),

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
