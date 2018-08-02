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
    with_http_cache({api_package, Name}, fun(Options) ->
        case hex_api:get_package(Name, Options ++ options()) of
            {ok, {200, _Headers, Payload}} ->
                {ok, Payload};

            Other ->
                Other
        end
    end).

get_repo_versions() ->
    with_http_cache(repo_versions, fun(Options) ->
        case hex_repo:get_versions(Options ++ options()) of
            {ok, {200, _Headers, Payload}} ->
                {ok, maps:get(packages, Payload)};

            Other ->
                Other
        end
    end).

get_repo_tarball(Name, Version) ->
    with_http_cache({repo_tarball, Name, Version}, fun(Options) ->
        case hex_repo:get_tarball(Name, Version, Options ++ options()) of
            {ok, {200, _Headers, Tarball}} ->
                {ok, Tarball};

            Other ->
                Other
        end
    end).

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    Options1 = hex_erl:default_options(),
    Options2 = put_http_options(Options1),
    Options3 = maybe_put_api_key(Options2),
    Options3.

put_http_options(Options) ->
    Fragment = <<"(myapp/1.0.0) (httpc)">>,
    lists:keystore(http_user_agent_fragment, 1, Options, {http_user_agent_fragment, Fragment}).

maybe_put_api_key(Options) ->
    case os:getenv("HEX_API_KEY") of
        false -> Options;
        Key -> lists:keystore(api_key, 1, Options, {api_key, Key})
    end.

with_http_cache(Key, Fun) ->
    ETag = get_cache({etag, Key}, <<"">>),
    Options = [{etag, ETag}],
    case Fun(Options) of
        {ok, {200, Headers, Body}} ->
            ETag = maps:get(<<"etag">>, Headers),
            ok = put_cache({etag, Key}, ETag),
            ok = put_cache({body, Key}, Body),
            {200, Headers, get_cache({body, Key}, undefined)};

        {ok, {304, Headers, _Body}} ->
            {200, Headers, get_cache({body, Key}, undefined)};

        Other ->
            Other
    end.

get_cache(Key, Default) ->
    application:get_env(myapp_hex, {cache, Key}, Default).

put_cache(Key, Value) ->
    application:set_env(myapp_hex, {cache, Key}, Value).
