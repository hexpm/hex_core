-module(hex_api_key).
-export([
    list/1,
    get/2,
    add/3,
    delete/2,
    delete_all/1
]).

list(Config) when is_map(Config) ->
    hex_api:get(["keys"], Config).

get(Name, Config) when is_map(Config) ->
    hex_api:get(["keys", Name], Config).

add(Name, Permissions, Config) when is_map(Config) ->
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    hex_api:post(["keys"], Params, Config).

delete(Name, Config) when is_map(Config) ->
    hex_api:delete(["keys", Name], Config).

delete_all(Config) when is_map(Config) ->
    hex_api:delete(["keys"], Config).
