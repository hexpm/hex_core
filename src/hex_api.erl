-module(hex_api).
-export([
    get_package/2,
    get_release/3,
    get_user/2,
    get_keys/1,
    get_key/2,
    search/3,
    get_owners/2
]).
-define(CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

-type search_params() :: [
    {sort, atom()} |
    {page, non_neg_integer()} |
    {description, binary()} |
    {extra, binary()}
].

%% @doc
%% Gets package.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_package(<<"package">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., #{
%%     %%=>     <<"name">> => <<"package1">>,
%%     %%=>     <<"meta">> => #{
%%     %%=>         <<"description">> => ...,
%%     %%=>         <<"licenses">> => ...,
%%     %%=>         <<"links">> => ...,
%%     %%=>         <<"maintainers">> => ...
%%     %%=>     },
%%     %%=>     ...,
%%     %%=>     <<"releases">> => [
%%     %%=>         #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%     %%=>         #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%     %%=>         ...
%%     %%=>     ]}}}
%% '''
%% @end
-spec get_package(binary(), hex_erl:options()) -> {ok, map()} | {error, term()}.
get_package(Name, Options) when is_binary(Name) and is_list(Options) ->
    get(<<"/packages/", Name/binary>>, Options).

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
-spec get_release(binary(), binary(), hex_erl:options()) -> {ok, map()} | {error, term()}.
get_release(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_list(Options) ->
    get(<<"/packages/", Name/binary, "/releases/", Version/binary>>, Options).

%% @doc
%% Gets user.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_user(<<"user">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., #{
%%     %%=>     <<"username">> => <<"user">>,
%%     %%=>     <<"packages">> => [
%%     %%=>         #{
%%     %%=>             <<"name">> => ...,
%%     %%=>             <<"url">> => ...,
%%     %%=>             ...
%%     %%=>         },
%%     %%=>         ...
%%     %%=>     ],
%%     %%=>     ...}}}
%% '''
%% @end
-spec get_user(binary(), hex_erl:options()) -> {ok, map()} | {error, term()}.
get_user(Username, Options) when is_binary(Username) and is_list(Options) ->
    get(<<"/users/", Username/binary>>, Options).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%%     hex_api:search(<<"package">>, [], hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"name">> => <<"package1">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
-spec search(binary(), search_params(), hex_erl:options()) -> {ok, [map()]} | {error, term()}.
search(Query, SearchParams, Options) when is_binary(Query) and is_list(SearchParams) and is_list(Options) ->
    QueryString = encode_query_string([{search, Query} | SearchParams]),
    get(<<"/packages?", QueryString/binary>>, Options).

%% Examples:
%%
%% ```
%%     hex_api:get_owners(<<"package">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"username">> => <<"alice">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
-spec get_owners(binary(), hex_erl:options()) -> {ok, [map()]} | {error, term()}.
get_owners(Name, Options) when is_binary(Name) and is_list(Options) ->
    get(<<"/packages/", Name/binary, "/owners">>, Options).

-spec get_keys(hex_erl:options()) -> {ok, [map()]} | {error, term()}.
get_keys(Options) when is_list(Options) ->
    get(<<"/keys">>, Options).

-spec get_key(binary(), hex_erl:options()) -> {ok, [map()]} | {error, term()}.
get_key(Name, Options) when is_list(Options) ->
    get(<<"/keys/", Name/binary>>, Options).

%%====================================================================
%% Internal functions
%%====================================================================

get(Path, Options) when is_binary(Path) and is_list(Options) ->
    URI = proplists:get_value(api_uri, Options),
    DefaultHeaders = make_headers(Options),
    ReqHeaders = maps:put(<<"accept">>, ?CONTENT_TYPE, DefaultHeaders),

    case hex_http:request(Options, get, <<URI/binary, Path/binary>>, ReqHeaders) of
        {ok, {Status, RespHeaders, Body} = Response} ->
            ContentType = maps:get(<<"content-type">>, RespHeaders),
            case binary:match(ContentType, ?CONTENT_TYPE) of
                {_, _} ->
                    {ok, {Status, RespHeaders, binary_to_term(Body)}};

                nomatch ->
                    Response
            end;

        Other ->
            Other
    end.

encode_query_string(List) ->
    QueryString =
        join("&",
            lists:map(fun
                ({K, V}) when is_atom(V) ->
                    atom_to_list(K) ++ "=" ++ atom_to_list(V);
                ({K, V}) when is_binary(V) ->
                    atom_to_list(K) ++ "=" ++ binary_to_list(V);
                ({K, V}) when is_integer(V) ->
                    atom_to_list(K) ++ "=" ++ integer_to_list(V)
            end, List)),
    Encoded = http_uri:encode(QueryString),
    list_to_binary(Encoded).

%% https://github.com/erlang/otp/blob/OTP-20.3/lib/stdlib/src/lists.erl#L1449:L1453
join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

%% TODO: copy-pasted from hex_repo
make_headers(Options) ->
    lists:foldl(fun set_header/2, #{}, Options).

set_header({api_key, Token}, Headers) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_Option, Headers) -> Headers.
