-module(hex_http).
-export([get/3]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("hex_erl.hrl").

-type adapter() :: module().
-type client() :: #{adapter => adapter(), user_agent_fragment => binary()}.
-type headers() :: #{binary() => binary()}.
-type status() :: non_neg_integer().

-callback get(URI :: binary(), headers()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.

-spec get(client(), string(), headers()) -> {ok, {status(), headers(), binary()}} | {error, term()}.
get(#{adapter := Adapter, user_agent_fragment := UserAgentFragment}, URI, Headers) when is_binary(URI) and is_map(Headers) ->
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:get(URI, Headers2).

user_agent(UserAgentFragment) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    iolist_to_binary(["hex_erl/", ?HEX_ERL_VERSION, " ", UserAgentFragment, OTPString]).

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
