-module(hex_http_httpc).
-behaviour(hex_http).
-export([request/3]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders) ->
    URI2 = binary_to_list(URI),
    ReqHeaders2 = headers_map_to_list(ReqHeaders),
    {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} =
        httpc:request(Method, {URI2, ReqHeaders2}, [], [{body_format, binary}]),
    RespHeaders2 = headers_list_to_map(RespHeaders),
    {ok, {StatusCode, RespHeaders2, RespBody}}.

%%====================================================================
%% Internal functions
%%====================================================================

headers_map_to_list(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc] end, [], Map).

headers_list_to_map(List) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc) end, #{}, List).
