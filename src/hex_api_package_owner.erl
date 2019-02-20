-module(hex_api_package_owner).
-export([
    add/3,
    delete/3,
    get/3,
    list/2
]).

%% Examples:
%%
%% ```
%% > hex_api_owner:list(hex_core:default_config(), <<"package">>).
%% {ok, {200, ..., [
%%     #{<<"username">> => <<"alice">>, ...},
%%     ...
%% ]}}
%% '''
list(Config, PackageName) when is_binary(PackageName) and is_map(Config) ->
    Path = hex_api:build_repository_path(Config, ["packages", PackageName, "owners"]),
    hex_api:get(Config, Path).

get(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    Path = hex_api:build_repository_path(Config, ["packages", PackageName, "owners", UsernameOrEmail]),
    hex_api:get(Config, Path).

add(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    Path = hex_api:build_repository_path(Config, ["packages", PackageName, "owners", UsernameOrEmail]),
    hex_api:put(Config, Path, #{}).

delete(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    Path = hex_api:build_repository_path(Config, ["packages", PackageName, "owners", UsernameOrEmail]),
    hex_api:delete(Config, Path).
