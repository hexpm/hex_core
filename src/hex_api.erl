-module(hex_api).
-export([
    delete/2,
    get/2,
    post/3,
    put/3
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

%%====================================================================
%% Internal functions
%%====================================================================

request(Method, PathSegments, Body, Options) when is_list(PathSegments) ->
    Path =
        erlang:iolist_to_binary(
            lists:join(<<"/">>,
                lists:map(fun http_uri:encode/1, PathSegments))),
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
