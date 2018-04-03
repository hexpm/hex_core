-module(hex_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-define(fixture(Path), element(2, file:read_file("test/fixtures/" ++ Path))).
-define(OPTIONS, [
    {client, #{adapter => hex_http_test, user_agent_fragment => <<"(test)">>}},
    {repo, #{uri => "https://repo.test", public_key => ?fixture("test_pub.pem")}},
    {verify, true}
]).
% -define(OPTIONS, hex_repo:default_options()).

get_names_test() ->
    {ok, #{packages := Packages}} = hex_repo:get_names(?OPTIONS),
    [#{name := <<"ecto">>}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_versions_test() ->
    {ok, #{packages := Packages}} = hex_repo:get_versions(?OPTIONS),
    [#{name := <<"ecto">>, versions := _}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_package_test() ->
    {ok, #{releases := Releases}} = hex_repo:get_package("ecto", ?OPTIONS),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {error, not_found} = hex_repo:get_package("nonexisting", ?OPTIONS),

    ok.

get_tarball_test() ->
    {ok, Tarball} = hex_repo:get_tarball("ecto", "1.0.0", ?OPTIONS),
    {ok, _} = hex_tarball:unpack(Tarball, memory),
    ok.
