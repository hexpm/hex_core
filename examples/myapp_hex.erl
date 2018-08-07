-module(myapp_hex).
-export([
    get_api_package/1,
    get_repo_tarball/2,
    get_repo_versions/0
]).

%%====================================================================
%% API functions
%%====================================================================

get_api_package(Name) ->
    Result = with_http_cache({api_package, Name}, fun(Config) ->
        hex_api_package:get(Name, maps:merge(Config, options()))
    end),
    case Result of
        {ok, {200, _Headers, Payload}} ->
            {ok, Payload};

        Other ->
            Other
    end.

get_repo_versions() ->
    Result = with_http_cache(repo_versions, fun(Config) ->
        hex_repo:get_versions(maps:merge(Config, options()))
    end),
    case Result of
        {ok, {200, _Headers, Payload}} ->
            {ok, maps:get(packages, Payload)};

        Other ->
            Other
    end.

get_repo_tarball(Name, Version) ->
    Result = with_http_cache({repo_tarball, Name, Version}, fun(Config) ->
        hex_repo:get_tarball(Name, Version, maps:merge(Config, options()))
    end),
    case Result of
        {ok, {200, _Headers, Tarball}} ->
            {ok, Tarball};

        Other ->
            Other
    end.

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    Config1 = hex_core:default_config(),
    Config2 = put_http_options(Config1),
    Config3 = maybe_put_api_key(Config2),
    Config3.

put_http_options(Config) ->
    maps:put(http_user_agent_fragment, <<"(myapp/1.0.0) (httpc)">>, Config).

maybe_put_api_key(Config) ->
    case os:getenv("HEX_API_KEY") of
        false -> Config;
        Key -> maps:put(api_key, Config, Key)
    end.

with_http_cache(Key, Fun) ->
    ETag = get_cache({etag, Key}, <<"">>),
    Config = #{http_etag => ETag},
    case Fun(Config) of
        {ok, {200, Headers, Body}} ->
            ok = put_cache({etag, Key}, ETag),
            ok = put_cache({body, Key}, Body),
            {200, Headers, Body};

        {ok, {304, Headers, _Body}} ->
            {200, Headers, get_cache({body, Key}, undefined)};

        Other ->
            Other
    end.

%% naive, process dictionary based cache. Don't use in production!
get_cache(Key, Default) ->
    case erlang:get({cache, Key}) of
        undefined ->
            Default;
        Other ->
            Other
    end.

put_cache(Key, Value) ->
    erlang:put({cache, Key}, Value),
    ok.
