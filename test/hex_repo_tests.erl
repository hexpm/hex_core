-module(hex_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-define(CONFIG, #{
    http_adapter => hex_http_test,
    http_user_agent_fragment => <<"(test)">>,
    repo_url => <<"https://repo.test">>,
    repo_public_key => hex_test_helpers:fixture("test_pub.pem"),
    repo_verify => true
}).
% -define(CONFIG, hex_core:default_config()).

get_names_test() ->
    {ok, {200, _, #{packages := Packages}}} = hex_repo:get_names(?CONFIG),
    [#{name := <<"ecto">>}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_versions_test() ->
    {ok, {200, _, #{packages := Packages}}} = hex_repo:get_versions(?CONFIG),
    [#{name := <<"ecto">>, versions := _}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_package_test() ->
    {ok, {200, _, #{releases := Releases}}} = hex_repo:get_package(?CONFIG, <<"ecto">>),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {ok, {403, _, _}} = hex_repo:get_package(?CONFIG, <<"nonexisting">>),
    ok.

get_tarball_test() ->
    {ok, {200, #{<<"etag">> := ETag}, Tarball}} = hex_repo:get_tarball(?CONFIG, <<"ecto">>, <<"1.0.0">>),
    {ok, _} = hex_tarball:unpack(Tarball, memory),

    {ok, {304, _, _}} = hex_repo:get_tarball(maps:put(http_etag, ETag, ?CONFIG), <<"ecto">>, <<"1.0.0">>),

    {ok, {403, _, _}} = hex_repo:get_tarball(?CONFIG, <<"ecto">>, <<"9.9.9">>),
    ok.
