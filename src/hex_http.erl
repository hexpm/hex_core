-module(hex_http).
-export([request/4]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("hex_erl.hrl").

-type method() :: get | post | put | patch | delete.
-type status() :: non_neg_integer().
-type headers() :: #{binary() => binary()}.

-callback request(method(), URI :: binary(), headers()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.

-spec request(hex_erl:options(), method(), string(), headers()) ->
    {ok, {status(), headers(), binary()}} | {error, term()}.
request(Options, Method, URI, Headers) when is_binary(URI) and is_map(Headers) ->
    Adapter = maps:get(http_adapter, Options),
    UserAgentFragment = maps:get(http_user_agent_fragment, Options),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:request(Method, URI, Headers2).

user_agent(UserAgentFragment) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    iolist_to_binary(["hex_erl/", ?HEX_ERL_VERSION, " ", UserAgentFragment, OTPString]).

%%====================================================================
%% Internal functions
%%====================================================================

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
