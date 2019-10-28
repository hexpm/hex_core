-module(hex_api_organization).
-export([
    get/1,
    list/1,
    update/2
]).

list(Config) when is_map(Config) ->
    hex_api:get(Config, ["orgs"]).

get(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, []),
    hex_api:get(Config, Path).

update(Config, Seats) when is_integer(Seats) and is_map(Config) ->
    Path = hex_api:build_organization_path(Config, []),
    Params = #{<<"seats">> => Seats},
    hex_api:post(Config, Path, Params).
