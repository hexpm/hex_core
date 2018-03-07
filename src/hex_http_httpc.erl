-module(hex_http_httpc).
-behaviour(hex_http).
-export([get/2, user_agent_string/0]).

get(URI, RequestHeaders) ->
    {ok, {{_, StatusCode, _}, ResponseHeaders, ResponseBody}} =
        httpc:request(get, {URI, maps:to_list(RequestHeaders)}, [], [{body_format, binary}]),
    {ok, {StatusCode, maps:from_list(ResponseHeaders), ResponseBody}}.

user_agent_string() ->
    "(httpc)".
