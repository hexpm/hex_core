-module(hex_http_httpc).
-behaviour(hex_http).
-export([get/2]).

%%====================================================================
%% API functions
%%====================================================================

get(URI, RequestHeaders) ->
    URI2 = binary_to_list(URI),
    RequestHeaders2 = headers_map_to_list(RequestHeaders),
    {ok, {{_, StatusCode, _}, ResponseHeaders, ResponseBody}} =
        httpc:request(get, {URI2, RequestHeaders2}, [], [{body_format, binary}]),
    ResponseHeaders2 = headers_list_to_map(ResponseHeaders),
    {ok, {StatusCode, ResponseHeaders2, ResponseBody}}.

%%====================================================================
%% Internal functions
%%====================================================================

headers_map_to_list(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc] end, [], Map).

headers_list_to_map(List) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc) end, #{}, List).
