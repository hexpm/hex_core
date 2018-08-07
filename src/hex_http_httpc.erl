-module(hex_http_httpc).
-behaviour(hex_http).
-export([request/4]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders, Body) ->
    Request = build_request(URI, ReqHeaders, Body),
    {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} =
        httpc:request(Method, Request, [], [{body_format, binary}]),
    RespHeaders2 = load_headers(RespHeaders),
    {ok, {StatusCode, RespHeaders2, RespBody}}.

%%====================================================================
%% Internal functions
%%====================================================================

build_request(URI, ReqHeaders, Body) ->
    build_request2(binary_to_list(URI), dump_headers(ReqHeaders), Body).

build_request2(URI, ReqHeaders, nil) ->
    {URI, ReqHeaders};
build_request2(URI, ReqHeaders, {ContentType, Body}) ->
    {URI, ReqHeaders, ContentType, Body}.

dump_headers(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), dump_value(V)} | Acc] end, [], Map).

dump_value(V) when is_integer(V) ->
    V;
dump_value(V) when is_binary(V) ->
    binary_to_list(V).

load_headers(List) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc) end, #{}, List).
