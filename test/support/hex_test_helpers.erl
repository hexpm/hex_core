-module(hex_test_helpers).
-export([in_tmp/1]).

in_tmp(Fun) ->
    {ok, Old} = file:get_cwd(),
    TmpDir = "tmp",
    ok = rebar_file_utils:rm_rf(TmpDir),
    ok = file:make_dir(TmpDir),
    Dir = TmpDir ++ "/test",
    ok = file:make_dir(Dir),
    file:set_cwd(Dir),
    Fun(),
    file:set_cwd(Old).
