-module(hex_api_key).
-export([
    list/1,
    get/2,
    add/3,
    delete/2,
    delete_all/1
]).

list(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys"]),
    hex_api:get(Config, Path).

get(Config, Name) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys", Name]),
    hex_api:get(Config, Path).

add(Config, Name, Permissions) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys"]),
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    hex_api:post(Config, Path, Params).

delete(Config, Name) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys", Name]),
    hex_api:delete(Config, Path).

delete_all(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys"]),
    hex_api:delete(Config, Path).
