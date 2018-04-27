-module(hex_api_tests).
-include_lib("eunit/include/eunit.hrl").
-define(OPTIONS, [
    {client, #{adapter => hex_http_test, user_agent_fragment => <<"(test)">>}},
    {uri, <<"https://api.test">>}
]).
% -define(OPTIONS, []).

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
