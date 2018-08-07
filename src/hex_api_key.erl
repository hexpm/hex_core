-module(hex_api_key).
-export([
    list/1,
    get/2,
    add/3,
    delete/2,
    delete_all/1
]).

list(Options) when is_map(Options) ->
    hex_api:get(<<"/keys">>, Options).

get(Name, Options) when is_map(Options) ->
    hex_api:get(<<"/keys/", Name/binary>>, Options).

add(Name, Permissions, Options) when is_map(Options) ->
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    hex_api:post(<<"/keys">>, Params, Options).

delete(Name, Options) when is_map(Options) ->
    hex_api:delete(<<"/keys/", Name/binary>>, Options).

delete_all(Options) when is_map(Options) ->
    hex_api:delete(<<"/keys">>, Options).

