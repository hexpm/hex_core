-module(hex_http).
-export([request/5]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("hex_core.hrl").

-type method() :: get | post | put | patch | delete.
-type status() :: non_neg_integer().
-type headers() :: #{binary() => binary()}.
-type body() :: nil.

-callback request(method(), URI :: binary(), headers(), body()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.

-spec request(hex_core:options(), method(), string(), headers(), body()) ->
    {ok, {status(), headers(), binary()}} | {error, term()}.
request(Options, Method, URI, Headers, Body) when is_binary(URI) and is_map(Headers) ->
    Adapter = maps:get(http_adapter, Options),
    UserAgentFragment = maps:get(http_user_agent_fragment, Options),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:request(Method, URI, Headers2, Body).

user_agent(UserAgentFragment) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    iolist_to_binary(["hex_erl/", ?HEX_CORE_VERSION, " ", UserAgentFragment, OTPString]).

%%====================================================================
%% Internal functions
%%====================================================================

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
