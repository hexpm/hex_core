-module(hex_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-define(OPTIONS, [
    {http_adapter, hex_http_test},
    {http_user_agent_fragment, <<"(test)">>},
    {repo_uri, <<"https://repo.test">>},
    {repo_public_key, hex_test_helpers:fixture("test_pub.pem")},
    {repo_verify, true}
]).
% -define(OPTIONS, hex_erl:default_options()).

get_names_test() ->
    {ok, {200, _, #{packages := Packages}}} = hex_repo:get_names(?OPTIONS),
    [#{name := <<"ecto">>}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_versions_test() ->
    {ok, {200, _, #{packages := Packages}}} = hex_repo:get_versions(?OPTIONS),
    [#{name := <<"ecto">>, versions := _}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_package_test() ->
    {ok, {200, _, #{releases := Releases}}} = hex_repo:get_package(<<"ecto">>, ?OPTIONS),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {ok, {403, _, _}} = hex_repo:get_package(<<"nonexisting">>, ?OPTIONS),
    ok.

get_tarball_test() ->
    {ok, {200, #{<<"etag">> := ETag}, Tarball}} = hex_repo:get_tarball(<<"ecto">>, <<"1.0.0">>, ?OPTIONS),
    {ok, _} = hex_tarball:unpack(Tarball, memory),

    {ok, {304, _, _}} = hex_repo:get_tarball(<<"ecto">>, <<"1.0.0">>, [{etag, ETag} | ?OPTIONS]),

    {ok, {403, _, _}} = hex_repo:get_tarball(<<"ecto">>, <<"9.9.9">>, ?OPTIONS),
    ok.
