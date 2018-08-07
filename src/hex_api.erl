-module(hex_api).
-export([
    delete/2,
    get/2,
    post/3,
    put/3,
    encode_query_string/1
]).
-define(ERL_CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

get(Path, Options) ->
    request(get, Path, nil, Options).

post(Path, Body, Options) ->
    request(post, Path, encode_body(Body), Options).

put(Path, Body, Options) ->
    request(put, Path, encode_body(Body), Options).

delete(Path, Options) ->
    request(delete, Path, nil, Options).

%% @private
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

%%====================================================================
%% Internal functions
%%====================================================================

request(Method, PathSegments, Body, Options) when is_list(PathSegments) ->
    Path =
        erlang:iolist_to_binary(
            join(<<"/">>,
                lists:map(fun encode/1, PathSegments))),
    request(Method, <<"/", Path/binary>>, Body, Options);
request(Method, Path, Body, Options) when is_binary(Path) and is_map(Options) ->
    DefaultHeaders = make_headers(Options),
    ReqHeaders = maps:merge(maps:get(http_headers, Options, #{}), DefaultHeaders),
    ReqHeaders2 = put_new(<<"accept">>, ?ERL_CONTENT_TYPE, ReqHeaders),

    case hex_http:request(Options, Method, build_url(Path, Options), ReqHeaders2, Body) of
        {ok, {Status, RespHeaders, RespBody} = Response} ->
            ContentType = maps:get(<<"content-type">>, RespHeaders, <<"">>),
            case binary:match(ContentType, ?ERL_CONTENT_TYPE) of
                {_, _} ->
                    {ok, {Status, RespHeaders, binary_to_term(RespBody)}};

                nomatch ->
                    Response
            end;

        Other ->
            Other
    end.

encode(Binary) when is_binary(Binary) ->
    encode(binary_to_list(Binary));
encode(String) when is_list(String) ->
    http_uri:encode(String).

build_url(Path, #{api_uri := URI, organization := Org}) when is_binary(Org) ->
    <<URI/binary, "/repos/", Org/binary, "/", Path/binary>>;
build_url(Path, #{api_uri := URI}) ->
    <<URI/binary, Path/binary>>.

encode_body({_ContentType, _Body} = Body) ->
    Body;
encode_body(Body) ->
    {binary_to_list(?ERL_CONTENT_TYPE), term_to_binary(Body)}.

%% TODO: copy-pasted from hex_repo
make_headers(Options) ->
    maps:fold(fun set_header/3, #{}, Options).

set_header(api_key, Token, Headers) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) -> Headers.

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.

%% https://github.com/erlang/otp/blob/OTP-20.3/lib/stdlib/src/lists.erl#L1449:L1453
join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].
