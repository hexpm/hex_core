-module(hex_api_organization_member).
-export([
    add/3,
    delete/2,
    get/2,
    list/1,
    update/3
]).

list(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["members"]),
    hex_api:get(Config, Path).

get(Config, UsernameOrEmail) when is_binary(UsernameOrEmail) and is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    hex_api:get(Config, Path).

add(Config, UsernameOrEmail, Role) when is_binary(UsernameOrEmail) and is_binary(Role) and is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["members"]),
    Params = #{<<"name">> => UsernameOrEmail, <<"role">> => Role},
    hex_api:post(Config, Path, Params).

update(Config, UsernameOrEmail, Role) when is_binary(UsernameOrEmail) and is_binary(Role) and is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    Params = #{<<"role">> => Role},
    hex_api:post(Config, Path, Params).

delete(Config, UsernameOrEmail) when is_binary(UsernameOrEmail) and is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["members", UsernameOrEmail]),
    hex_api:delete(Config, Path).
