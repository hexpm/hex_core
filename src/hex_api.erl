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

-type options() :: [{client, hex_http:client()} | {uri, binary()}].

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

        {ok, {404, _, _Body}} ->
            {error, not_found};

        Other ->
            Other
    end.

%% @doc
%% Gets package.
%% @end
-spec get_package(binary()) -> {ok, map()} | {error, term()}.
get_package(Name) when is_binary(Name) ->
    get_package(Name, []).

-spec get_package(binary(), options()) -> {ok, map()} | {error, term()}.
get_package(Name, Options) when is_binary(Name) and is_list(Options) ->
    Fields = [
        name,
        repository,
        url,
        docs_html_url,
        html_url,
        {releases, [url, version]},
        retirements,
        {meta, [maintainers, links, licenses, description]},
        {owners, [username, url, email]},
        {downloads, [week, recent, day, all]},
        inserted_at,
        updated_at
    ],
    Body = get(<<"/packages/", Name/binary>>, merge_with_default_options(Options)),
    decode_body(Body, Fields).

%% @doc
%% Gets package release.
%% @end
-spec get_release(binary(), binary()) -> {ok, map()} | {error, term()}.
get_release(Name, Version) when is_binary(Name) and is_binary(Version) ->
    get_release(Name, Version, []).

-spec get_release(binary(), binary(), options()) -> {ok, map()} | {error, term()}.
get_release(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_list(Options) ->
    Fields = [
        version,
        url,
        package_url,
        html_url,
        docs_html_url,
        {retirement, [reason, message]},
        {requirements, {map, [app, optional, requirement]}},
        {meta, [elixir, build_tools, app]},
        has_docs,
        downloads,
        inserted_at,
        updated_at
    ],
    Body = get(<<"/packages/", Name/binary, "/releases/", Version/binary>>, merge_with_default_options(Options)),
    decode_body(Body, Fields).

%% @doc
%% Gets user.
%% @end
-spec get_user(binary()) -> {ok, map()} | {error, term()}.
get_user(Username) when is_binary(Username) ->
    get_user(Username, []).

-spec get_user(binary(), options()) -> {ok, map()} | {error, term()}.
get_user(Username, Options) when is_binary(Username) and is_list(Options) ->
    Fields = [
        username,
        url,
        {packages, [name, repository, url, html_url]},
        handles,
        full_name,
        email,
        inserted_at,
        updated_at
    ],
    Body = get(<<"/users/", Username/binary>>, merge_with_default_options(Options)),
    decode_body(Body, Fields).

%%====================================================================
%% Internal functions
%%====================================================================

decode_body({ok, Body}, Fields) ->
    {ok, decode(Body, Fields)};
decode_body(Other, _) ->
    Other.

decode(Map, Fields) when is_map(Map), is_list(Fields) ->
    lists:foldl(fun(Key, Acc) -> put_value(Key, Map, Acc) end, #{}, Fields);
decode(List, Fields) when is_list(List), is_list(Fields) ->
    List2 = lists:foldl(fun(Value, Acc) -> [decode(Value, Fields) | Acc] end, [], List),
    lists:reverse(List2);
decode(Map, {map, Fields}) ->
    maps:fold(fun(Key, Value, Acc) -> maps:put(Key, decode(Value, Fields), Acc) end, #{}, Map);
decode(nil, _Fields) ->
    nil.

put_value({Key, InnerFields}, Map, Acc) ->
    case maps:find(atom_to_binary(Key, utf8), Map) of
        {ok, Value} -> maps:put(Key, decode(Value, InnerFields), Acc);
        error -> Acc
    end;
put_value(Key, Map, Acc) ->
    case maps:find(atom_to_binary(Key, utf8), Map) of
        {ok, Value} -> maps:put(Key, Value, Acc);
        error -> Acc
    end.

merge_with_default_options(Options) when is_list(Options) ->
    lists:ukeymerge(1, lists:sort(Options), default_options()).
