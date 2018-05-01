-module(hex_api_tests).
-include_lib("eunit/include/eunit.hrl").
% -define(OPTIONS, [
%     {client, #{adapter => hex_http_test, user_agent_fragment => <<"(test)">>}},
%     {uri, <<"https://api.test">>},
%     {api_key, <<"dummy">>}
% ]).
-define(OPTIONS, [{api_key, hex_test_helpers:api_key()}]).

get_package_test() ->
    {ok, Package} = hex_api:get_package(<<"ecto">>, ?OPTIONS),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,
    ok.

get_release_test() ->
    {ok, Release} = hex_api:get_release(<<"ecto">>, <<"1.0.0">>, ?OPTIONS),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{<<"decimal">> := #{
        <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>}} = Requirements,
    ok.

get_user_test() ->
    {ok, User} = hex_api:get_user(<<"josevalim">>, ?OPTIONS),
    #{<<"username">> := <<"josevalim">>, <<"packages">> := _} = User,
    ok.

search_test() ->
    {ok, [Package | _]} = hex_api:search(<<"ecto">>, [{sort, downloads}, {page, 1}], ?OPTIONS),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,
    ok.

get_owners_test() ->
    {ok, [Owner | _]} = hex_api:get_owners(<<"decimal">>, ?OPTIONS),
    <<"ericmj">> = maps:get(<<"username">>, Owner),
    ok.
