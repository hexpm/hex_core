-module(hex_api_tests).
-include_lib("eunit/include/eunit.hrl").
-define(OPTIONS, #{
    http_adapter => hex_http_test,
    http_user_agent_fragment => <<"(test)">>,
    api_uri => <<"https://api.test">>,
    api_key => <<"dummy">>
}).
% -define(OPTIONS, maps:put(api_key, hex_test_helpers:api_key(), hex_erl:default_options())).

get_package_test() ->
    {ok, {200, _, Package}} = hex_api:get_package(<<"ecto">>, ?OPTIONS),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,

    {ok, {404, _, #{}}} = hex_api:get_package(<<"nonexisting">>, ?OPTIONS),
    ok.

get_release_test() ->
    {ok, {200, _, Release}} = hex_api:get_release(<<"ecto">>, <<"1.0.0">>, ?OPTIONS),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{<<"decimal">> := #{
        <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>}} = Requirements,
    ok.

get_user_test() ->
    {ok, {200, _, User}} = hex_api:get_user(<<"josevalim">>, ?OPTIONS),
    #{<<"username">> := <<"josevalim">>, <<"packages">> := _} = User,
    ok.

search_test() ->
    {ok, {200, _, [Package | _]}} = hex_api:search(<<"ecto">>, [{sort, downloads}, {page, 1}], ?OPTIONS),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,
    ok.

owner_test() ->
    {ok, {200, _, [Owner | _]}} = hex_api:get_owners(<<"decimal">>, ?OPTIONS),
    <<"ericmj">> = maps:get(<<"username">>, Owner),
    ok.

keys_test() ->
    {ok, {200, _, [Key | _]}} = hex_api:get_keys(?OPTIONS),
    #{<<"name">> := Name} = Key,

    {ok, {200, _, Key}} = hex_api:get_key(Name, ?OPTIONS),
    ok.
