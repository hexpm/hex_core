-module(hex_api_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(CONFIG, #{
    http_adapter => hex_http_test,
    http_user_agent_fragment => <<"(test)">>,
    api_url => <<"https://api.test">>,
    api_key => <<"dummy">>
}).
% -define(CONFIG, maps:put(api_key, hex_test_helpers:api_key(), hex_core:default_config())).

suite() ->
    [{require, {ssl_certs, [test_pub, test_priv]}}].

all() ->
    [package_test, release_test, user_test, owner_test, keys_test].

package_test(_Config) ->
    {ok, {200, _, Package}} = hex_api_package:get(?CONFIG, <<"ecto">>),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,

    {ok, {404, _, #{}}} = hex_api_package:get(?CONFIG, <<"nonexisting">>),

    {ok, {200, _, [Package | _]}} = hex_api_package:search(?CONFIG, <<"ecto">>, [{sort, downloads}, {page, 1}]),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,
    ok.

release_test(_Config) ->
    {ok, {200, _, Release}} = hex_api_release:get(?CONFIG, <<"ecto">>, <<"1.0.0">>),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{<<"decimal">> := #{
        <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>}} = Requirements,
    ok.

user_test(_Config) ->
    {ok, {200, _, User}} = hex_api_user:get(?CONFIG, <<"josevalim">>),
    #{<<"username">> := <<"josevalim">>, <<"packages">> := _} = User,
    ok.

owner_test(_Config) ->
    {ok, {200, _, [Owner | _]}} = hex_api_package_owner:list(?CONFIG, <<"decimal">>),
    <<"ericmj">> = maps:get(<<"username">>, Owner),
    ok.

keys_test(_Config) ->
    {ok, {200, _, [Key | _]}} = hex_api_key:list(?CONFIG),
    #{<<"name">> := Name} = Key,

    {ok, {200, _, Key}} = hex_api_key:get(?CONFIG, Name),

    Permissions = [#{<<"domain">> => <<"api">>, <<"resource">> => <<"read">>}],
    {ok, {201, _, Key2}} = hex_api_key:add(?CONFIG, Name, Permissions),
    #{<<"name">> := Name2} = Key2,

    {ok, {200, _, #{<<"name">> := Name2}}} = hex_api_key:delete(?CONFIG, Name2),
    ok.
