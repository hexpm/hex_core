-module(hex_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-define(fixture(Path), element(2, file:read_file("test/fixtures/" ++ Path))).
-define(CLIENT, #{adapter => hex_http_test, user_agent_string => "(test)"}).
-define(REPO, #{uri => "https://repo.test", public_key => ?fixture("test_pub.pem")}).
% -define(CLIENT, #{adapter => hex_http_httpc, user_agent_string => "(httpc)"}).
% -define(REPO, #{uri => "https://repo.hex.pm", public_key => ?fixture("hexpm_pub.pem")}).

get_names_test() ->
    {ok, #{packages := Packages}} = hex_repo:get_names(?CLIENT, ?REPO),
    [#{name := <<"ecto">>}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_versions_test() ->
    {ok, #{packages := Packages}} = hex_repo:get_versions(?CLIENT, ?REPO, [{verify, false}]),
    [#{name := <<"ecto">>, versions := _}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_package_test() ->
    {ok, #{releases := Releases}} = hex_repo:get_package(?CLIENT, ?REPO, "ecto", [{verify, false}]),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {error, not_found} = hex_repo:get_package(?CLIENT, ?REPO, "nonexisting"),

    ok.

get_tarball_test() ->
    {ok, Tarball} = hex_repo:get_tarball(?CLIENT, ?REPO, "ecto", "1.0.0"),
    {ok, _} = hex_tarball:unpack(Tarball, memory),
    ok.
