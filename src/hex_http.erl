-module(hex_http).
-export([get/3]).

-type adapter() :: module().
-type status() :: non_neg_integer().
-type headers() :: map().

-callback get(URI :: string(), headers()) ->
    {ok, status(), headers(), binary()} |
    {error, term()}.
-callback user_agent_string() -> string().

-spec get(adapter(), string(), headers()) -> {ok, {status(), headers(), binary()}} | {error, term()}.
get(Adapter, URI, Headers) ->
    Headers2 = put_new("user-agent", user_agent(Adapter), Headers),
    Adapter:get(URI, Headers2).

user_agent(Adapter) ->
    OtpVersion = erlang:system_info(otp_release),
    UserAgentString = Adapter:user_agent_string(),
    "hex_erl/0.1.0 " ++ UserAgentString ++ " (OTP/" ++ OtpVersion ++ ")".

put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
