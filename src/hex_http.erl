-module(hex_http).
-export([get/3]).

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
    OtpVersion = erlang:system_info(otp_release),
    "hex_erl/0.1.0 " ++ UserAgentString ++ " (OTP/" ++ OtpVersion ++ ")".

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
