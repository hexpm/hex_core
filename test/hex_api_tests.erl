-module(hex_api_tests).
-include_lib("eunit/include/eunit.hrl").
-define(OPTIONS, #{
    http_adapter => hex_http_test,
    http_user_agent_fragment => <<"(test)">>,
    api_url => <<"https://api.test">>,
    api_key => <<"dummy">>
}).
% -define(OPTIONS, maps:put(api_key, hex_test_helpers:api_key(), hex_core:default_config())).

package_test() ->
    {ok, {200, _, Package}} = hex_api_package:get(<<"ecto">>, ?OPTIONS),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,

    {ok, {404, _, #{}}} = hex_api_package:get(<<"nonexisting">>, ?OPTIONS),

    {ok, {200, _, [Package | _]}} = hex_api_package:search(<<"ecto">>, [{sort, downloads}, {page, 1}], ?OPTIONS),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,
    ok.

release_test() ->
    {ok, {200, _, Release}} = hex_api_release:get(<<"ecto">>, <<"1.0.0">>, ?OPTIONS),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{<<"decimal">> := #{
        <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>}} = Requirements,
    ok.

user_test() ->
    {ok, {200, _, User}} = hex_api_user:get(<<"josevalim">>, ?OPTIONS),
    #{<<"username">> := <<"josevalim">>, <<"packages">> := _} = User,
    ok.

owner_test() ->
    {ok, {200, _, [Owner | _]}} = hex_api_package_owner:list(<<"decimal">>, ?OPTIONS),
    <<"ericmj">> = maps:get(<<"username">>, Owner),
    ok.

keys_test() ->
    {ok, {200, _, [Key | _]}} = hex_api_key:list(?OPTIONS),
    #{<<"name">> := Name} = Key,

    {ok, {200, _, Key}} = hex_api_key:get(Name, ?OPTIONS),

    Permissions = [#{<<"domain">> => <<"api">>, <<"resource">> => <<"read">>}],
    {ok, {201, _, Key2}} = hex_api_key:add(Name, Permissions, ?OPTIONS),
    #{<<"name">> := Name2} = Key2,

    {ok, {200, _, #{<<"name">> := Name2}}} = hex_api_key:delete(Name2, ?OPTIONS),
    ok.
