-module(hex_api).
-export([
    default_options/0,
    get/1,
    get/2,
    get_package/1,
    get_package/2,
    get_release/2,
    get_release/3,
    get_user/1,
    get_user/2
]).

-type options() :: [{client, hex_http:client()} | {uri, string()}].

%% @doc
%% Default options used to interact with the API.
%% @end
-spec default_options() -> options().
default_options() ->
    Client = #{adapter => hex_http_httpc, user_agent_fragment => <<"(httpc)">>},
    URI = <<"https://hex.pm/api">>,
    [{client, Client}, {uri, URI}].

-spec get(binary()) -> {ok, term()} | {error, term()}.
get(Path) when is_binary(Path) ->
    get(Path, default_options()).

-spec get(binary(), options()) -> {ok, term()} | {error, term()}.
get(Path, Options) when is_binary(Path) and is_list(Options) ->
    Client = proplists:get_value(client, Options),
    URI = proplists:get_value(uri, Options),
    Headers = #{<<"accept">> => <<"application/vnd.hex+erlang">>},

    case hex_http:get(Client, <<URI/binary, Path/binary>>, Headers) of
        {ok, {200, _, Body}} ->
            {ok, binary_to_term(Body)};

        Other ->
            Other
    end.

%% @doc
%% Gets package.
%% @end
-spec get_package(binary()) -> {ok, map()} | {error, term()}.
get_package(Name) when is_binary(Name) ->
    get_package(Name, default_options()).

-spec get_package(binary(), options()) -> {ok, map()} | {error, term()}.
get_package(Name, Options) when is_binary(Name) and is_list(Options) ->
    get(<<"/packages/", Name/binary>>, Options).

%% @doc
%% Gets package release.
%% @end
-spec get_release(binary(), binary()) -> {ok, map()} | {error, term()}.
get_release(Name, Version) when is_binary(Name) and is_binary(Version) ->
    get_release(Name, Version, []).

-spec get_release(binary(), binary(), options()) -> {ok, map()} | {error, term()}.
get_release(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_list(Options) ->
    get(<<"/packages/", Name/binary, "/releases/", Version/binary>>, merge_with_default_options(Options)).

%% @doc
%% Gets user.
%% @end
-spec get_user(binary()) -> {ok, map()} | {error, term()}.
get_user(Username) when is_binary(Username) ->
    get_user(Username, []).

-spec get_user(binary(), options()) -> {ok, map()} | {error, term()}.
get_user(Username, Options) when is_binary(Username) and is_list(Options) ->
    DeprecatedFields = [<<"owned_packages">>],

    case get(<<"/users/", Username/binary>>, merge_with_default_options(Options)) of
        {ok, Map} ->
            {ok, maps:without(DeprecatedFields, Map)};

        Other ->
            Other
    end.

%%====================================================================
%% Internal functions
%%====================================================================

merge_with_default_options(Options) when is_list(Options) ->
    lists:ukeymerge(1, lists:sort(Options), default_options()).
