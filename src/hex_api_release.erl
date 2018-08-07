-module(hex_api_release).
-export([
    get/3,
    retire/4,
    unretire/3
]).

%% @doc
%% Gets package release.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_release(<<"package">>, <<"1.0.0">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., #{
%%     %%=>     <<"version">> => <<"1.0.0">>,
%%     %%=>     <<"meta">> => #{
%%     %%=>         <<"description">> => ...,
%%     %%=>         <<"licenses">> => ...,
%%     %%=>         <<"links">> => ...,
%%     %%=>         <<"maintainers">> => ...
%%     %%=>     },
%%     %%=>     ...}}}
%% '''
%% @end
get(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    hex_api:get(<<"/packages/", Name/binary, "/releases/", Version/binary>>, Options).

retire(Name, Version, Params, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    hex_api:post(<<"/packages/", Name/binary, "/releases/", Version/binary, "/retire">>, Params, Options).

unretire(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    hex_api:delete(<<"/packages/", Name/binary, "/releases/", Version/binary, "/retire">>, Options).
