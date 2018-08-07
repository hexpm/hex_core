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
%%     hex_api_owner:list(hex_core:default_config(), <<"package">>).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"username">> => <<"alice">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
list(Config, PackageName) when is_binary(PackageName) and is_map(Config) ->
    hex_api:get(Config, ["packages", PackageName, "owners"]).

get(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    hex_api:get(Config, ["packages", PackageName, "owners", UsernameOrEmail]).

add(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    hex_api:put(Config, ["packages", PackageName, "owners", UsernameOrEmail], #{}).

delete(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    hex_api:delete(Config, ["packages", PackageName, "owners", UsernameOrEmail]).
