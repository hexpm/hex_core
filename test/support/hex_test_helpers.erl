-module(hex_test_helpers).
-export([in_tmp/1, fixture/1, api_key/0]).

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

fixture(Path) ->
    TestDir = filename:dirname(filename:dirname(?FILE)),
    FixtureDir = filename:join(TestDir, "fixtures"),
    Path2 = filename:join(FixtureDir, Path),
    {ok, Binary} = file:read_file(Path2),
    Binary.

api_key() ->
    case os:getenv("TEST_API_KEY") of
        false -> nil;
        Token -> list_to_binary(Token)
    end.
