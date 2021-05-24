%% @doc
%% httpc-based implementation of `hex_http' contract.
%%
%% Configuration keys:
%%
%% * `profile' - the name of the profile, defaults to `default'. See <a
%% href="http://erlang.org/doc/man/httpc.html#set_options-2">`httpc:set_options/2'</a>
%% for more information on setting options on profiles.
%%
%% * `http_options' - a list of HTTP options, defaults to `[]'. See <a
%% href="http://erlang.org/doc/man/httpc.html#request-5">`httpc:request/5'</a>
%% for a list of available HTTP options.

-module(hex_http_httpc).
-behaviour(hex_http).
-export([request/5]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders, Body, AdapterConfig) ->
    Profile = maps:get(profile, AdapterConfig, default),
    HTTPOptions = maps:get(http_options, AdapterConfig, []),

    case proplists:get_value(ssl, HTTPOptions) of
      undefined ->
        io:format("[hex_http_httpc] using default ssl options which are insecure.~n"
                  "Configure your adapter with: "
                  "{hex_http_httpc, #{http_options => [{ssl, SslOpts}]}}~n");
      _ ->
        ok
    end,

    Request = build_request(URI, ReqHeaders, Body),
    case httpc:request(Method, Request, HTTPOptions, [{body_format, binary}], Profile) of
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            RespHeaders2 = load_headers(RespHeaders),
            {ok, {StatusCode, RespHeaders2, RespBody}};
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
build_request(URI, ReqHeaders, Body) ->
    build_request2(binary_to_list(URI), dump_headers(ReqHeaders), Body).

%% @private
build_request2(URI, ReqHeaders, undefined) ->
    {URI, ReqHeaders};
build_request2(URI, ReqHeaders, {ContentType, Body}) ->
    {URI, ReqHeaders, ContentType, Body}.

%% @private
dump_headers(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc] end, [], Map).

%% @private
load_headers(List) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc) end, #{}, List).
