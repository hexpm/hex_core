-module(hex_http).
-export([get/3]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("hex_erl.hrl").

-type client() :: #{adapter => module(), user_agent_string => string()}.
-type status() :: non_neg_integer().
-type headers() :: map().

-callback get(URI :: string(), headers()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.

-spec get(client(), string(), headers()) -> {ok, {status(), headers(), binary()}} | {error, term()}.
get(#{adapter := Adapter, user_agent_string := UserAgentString}, URI, Headers) ->
    Headers2 = put_new("user-agent", user_agent(UserAgentString), Headers),
    Adapter:get(URI, Headers2).

user_agent(UserAgentString) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    "hex_erl/" ++ ?HEX_ERL_VERSION ++ " " ++ UserAgentString ++ OTPString.

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
