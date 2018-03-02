-module(hex_tarball_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

memory_test() ->
    Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Contents),
    {ok, #{checksum := Checksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory),
    ok.

disk_test() ->
    in_tmp(fun() ->
        ok = file:write_file("foo.erl", <<"-module(foo).">>),

        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
        Files = ["foo.erl"],
        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files),
        <<"90F7D8E898A44226EE61CF7229D49D2620D08EDF88B64230D0FA197F2D8A42B6">> = hex_tarball:format_checksum(Checksum),
        {ok, #{checksum := Checksum, metadata := Metadata}} = hex_tarball:unpack(Tarball, "unpack"),
        {ok, <<"-module(foo).">>} = file:read_file("unpack/foo.erl")
    end).

timestamps_and_permissions_test() ->
    in_tmp(fun() ->
        Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

        ok = file:write_file("foo.sh", <<"">>),
        {ok, FileInfo} = file:read_file_info("foo.sh"),
        ok = file:write_file_info("foo.sh", FileInfo#file_info{mode=8#100755}),
        {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, [{"foo.erl", <<"">>}, "foo.sh"]),

        %% inside tarball
        {ok, Files} = hex_erl_tar:extract({binary, Tarball}, [memory]),
        {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, Files),
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
        [{{2000,1,1}, {0,0,0}}] = calendar:local_time_to_universal_time_dst(FooErlFileInfo#file_info.mtime),
        [{{2000,1,1}, {0,0,0}}] = calendar:local_time_to_universal_time_dst(FooShFileInfo#file_info.mtime)
    end).

%%====================================================================
%% Helpers
%%====================================================================

in_tmp(F) ->
    Old = file:get_cwd(),
    TmpDir = "tmp",
    ok = rm_rf(TmpDir),
    ok = file:make_dir(TmpDir),
    Dir = TmpDir ++ "/" ++ integer_to_list(erlang:unique_integer()),
    ok = file:make_dir(Dir),
    file:set_cwd(Dir),
    apply(F, []),
    file:set_cwd(Old).

rm_rf(Path) ->
    [] = os:cmd("rm -rf " ++ Path),
    ok.

epoch() ->
    NixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Y2kEpoch = calendar:datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}}),
    Y2kEpoch - NixEpoch.
