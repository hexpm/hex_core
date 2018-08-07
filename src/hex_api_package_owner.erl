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
%%     hex_api_owner:list(<<"package">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"username">> => <<"alice">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
list(PackageName, Options) when is_binary(PackageName) and is_map(Options) ->
    hex_api:get(<<"/packages/", PackageName/binary, "/owners">>, Options).

get(PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    hex_api:get(<<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, Options).

add(PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    hex_api:put(<<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, #{}, Options).

delete(PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    hex_api:delete(<<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, Options).
