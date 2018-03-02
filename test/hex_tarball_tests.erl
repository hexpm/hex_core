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
        <<"0232E2D6E064728FC028947BE3B4070065E577E2EAFD16697593635FABAE4702">> = hex_tarball:format_checksum(Checksum),
        {ok, #{checksum := Checksum, metadata := Metadata}} = hex_tarball:unpack(Tarball, "unpack"),
        {ok, <<"-module(foo).">>} = file:read_file("unpack/foo.erl")
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
