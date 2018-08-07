-module(hex_api_release).
-export([
    delete/3,
    get/3,
    publish/2,
    retire/4,
    unretire/3
]).

%% @doc
%% Gets package release.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_release(<<"package">>, <<"1.0.0">>, hex_core:default_options()).
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
    hex_api:get(["packages", Name, "releases", Version], Options).

publish(Tarball, Options) when is_binary(Tarball) and is_map(Options) ->
    TarballContentType = "application/octet-stream",
    Options2 = put_header(<<"content-length">>, integer_to_binary(byte_size(Tarball)), Options),
    Body = {TarballContentType, Tarball},
    hex_api:post(["publish"], Body, Options2).

delete(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    hex_api:delete(["packages", Name, "releases", Version], Options).

retire(Name, Version, Params, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    hex_api:post(["packages", Name, "releases", Version, "retire"], Params, Options).

unretire(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    hex_api:delete(["packages", Name, "releases", Version, "retire"], Options).

%%====================================================================
%% Internal functions
%%====================================================================

put_header(Name, Value, Options) ->
    Headers = maps:get(http_headers, Options, #{}),
    Headers2 = maps:put(Name, Value, Headers),
    maps:put(http_headers, Headers2, Options).
