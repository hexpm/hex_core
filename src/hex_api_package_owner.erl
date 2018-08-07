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
%%     hex_api_owner:list(<<"package">>, hex_core:default_config()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"username">> => <<"alice">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
list(PackageName, Config) when is_binary(PackageName) and is_map(Config) ->
    hex_api:get(["packages", PackageName, "owners"], Config).

get(PackageName, UsernameOrEmail, Config) when is_binary(PackageName) and is_map(Config) ->
    hex_api:get(["packages", PackageName, "owners", UsernameOrEmail], Config).

add(PackageName, UsernameOrEmail, Config) when is_binary(PackageName) and is_map(Config) ->
    hex_api:put(["packages", PackageName, "owners", UsernameOrEmail], #{}, Config).

delete(PackageName, UsernameOrEmail, Config) when is_binary(PackageName) and is_map(Config) ->
    hex_api:delete(["packages", PackageName, "owners", UsernameOrEmail], Config).
