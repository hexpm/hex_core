-module(hex_cli_auth_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_HTTP_ADAPTER_CONFIG, #{profile => default}).

-define(CONFIG, (hex_core:default_config())#{
    http_adapter => {hex_http_test, ?DEFAULT_HTTP_ADAPTER_CONFIG},
    http_user_agent_fragment => <<"(test)">>,
    api_url => <<"https://api.test">>,
    repo_url => <<"https://repo.test">>,
    repo_name => <<"hexpm">>,
    repo_public_key => ct:get_config({ssl_certs, test_pub})
}).

suite() ->
    [{require, {ssl_certs, [test_pub, test_priv]}}].

all() ->
    [
        %% resolve_api_auth tests
        resolve_api_auth_config_passthrough_test,
        resolve_api_auth_per_repo_test,
        resolve_api_auth_parent_repo_test,
        resolve_api_auth_oauth_test,
        resolve_api_auth_oauth_expired_refresh_test,
        resolve_api_auth_oauth_no_refresh_token_test,
        resolve_api_auth_no_auth_test,

        %% resolve_repo_auth tests - trusted vs untrusted
        resolve_repo_auth_config_passthrough_test,
        resolve_repo_auth_callback_repo_key_test,
        resolve_repo_auth_trusted_auth_key_test,
        resolve_repo_auth_untrusted_ignores_auth_key_test,
        resolve_repo_auth_oauth_fallback_test,
        resolve_repo_auth_no_auth_test,

        %% resolve_repo_auth tests - token exchange
        resolve_repo_auth_oauth_exchange_new_token_test,
        resolve_repo_auth_oauth_exchange_existing_valid_test,
        resolve_repo_auth_oauth_exchange_existing_expired_test,
        resolve_repo_auth_parent_repo_auth_key_test,

        %% with_api tests - OTP handling
        with_api_otp_required_test,
        with_api_otp_invalid_retry_test,
        with_api_otp_cancelled_test,
        with_api_otp_max_retries_test,

        %% with_api tests - token refresh on 401
        with_api_token_expired_refresh_test,

        %% with_api tests - reauthentication after refresh failure
        with_api_token_expired_reauth_yes_test,
        with_api_token_expired_reauth_no_test,
        with_api_token_expired_reauth_inline_false_test,

        %% with_api tests - wrapper behavior
        with_api_optional_test,
        with_api_optional_token_refresh_failed_test,
        with_api_auth_inline_test,
        with_api_device_auth_test,

        %% with_repo tests - wrapper behavior
        with_repo_optional_test,
        with_repo_trusted_with_auth_test,
        with_repo_optional_token_refresh_failed_test,

        %% concurrency tests
        resolve_oauth_token_concurrent_refresh_serialized_test,
        resolve_oauth_token_refresh_failure_clears_once_test,
        device_auth_concurrent_serialized_reuses_login_test
    ].

%%====================================================================
%% Test Cases - resolve_api_auth
%%====================================================================

resolve_api_auth_config_passthrough_test(_Config) ->
    %% When api_key is already in config, it should be used directly
    Config = config_with_callbacks(#{}),
    ConfigWithKey = Config#{api_key => <<"config_api_key">>},

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(read, ConfigWithKey),
    ?assertEqual(<<"config_api_key">>, ApiKey),
    ?assertEqual(#{source => config, has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_per_repo_test(_Config) ->
    %% Test per-repo api_key from callback
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{api_key => <<"repo_api_key">>}}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(write, Config),
    ?assertEqual(<<"repo_api_key">>, ApiKey),
    ?assertEqual(#{source => config, has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_parent_repo_test(_Config) ->
    %% Test parent repo fallback for "hexpm:org" repos
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{api_key => <<"parent_api_key">>}}
    }),

    {ok, ApiKey, _} = hex_cli_auth:resolve_api_auth(
        write, Config#{repo_name => <<"hexpm:myorg">>}
    ),
    ?assertEqual(<<"parent_api_key">>, ApiKey),
    ok.

resolve_api_auth_oauth_test(_Config) ->
    %% Test OAuth token fallback with valid token
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"oauth_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(read, Config),
    ?assertEqual(<<"Bearer oauth_token">>, ApiKey),
    ?assertEqual(#{source => oauth, has_refresh_token => true}, AuthContext),
    ok.

resolve_api_auth_oauth_expired_refresh_test(_Config) ->
    %% Test OAuth token refresh when expired
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                %% Expired
                expires_at => Now - 100
            }},
        persist_oauth_tokens => fun(Scope, Access, Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Refresh, Expires},
            ok
        end
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(read, Config),
    %% Should have refreshed and got a new token
    ?assertMatch(<<"Bearer ", _/binary>>, ApiKey),
    ?assertEqual(#{source => oauth, has_refresh_token => true}, AuthContext),

    %% Verify token was persisted
    receive
        {persisted, global, _NewAccess, _NewRefresh, _NewExpires} -> ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

resolve_api_auth_oauth_no_refresh_token_test(_Config) ->
    %% Test OAuth token without refresh token
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"oauth_token">>,
                expires_at => Now + 3600
            }}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(read, Config),
    ?assertEqual(<<"Bearer oauth_token">>, ApiKey),
    ?assertEqual(#{source => oauth, has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_no_auth_test(_Config) ->
    %% Test when no auth is available
    Config = config_with_callbacks(#{
        auth_config => #{},
        oauth_tokens => error
    }),

    Result = hex_cli_auth:resolve_api_auth(read, Config),
    ?assertEqual({error, no_auth}, Result),
    ok.

%%====================================================================
%% Test Cases - resolve_repo_auth
%%====================================================================

resolve_repo_auth_config_passthrough_test(_Config) ->
    %% When repo_key is already in config, it should be used directly
    Config = config_with_callbacks(#{}),
    ConfigWithKey = Config#{repo_key => <<"config_repo_key">>},

    {ok, RepoKey, AuthContext} = hex_cli_auth:resolve_repo_auth(ConfigWithKey),
    ?assertEqual(<<"config_repo_key">>, RepoKey),
    ?assertEqual(#{source => config, has_refresh_token => false}, AuthContext),
    ok.

resolve_repo_auth_callback_repo_key_test(_Config) ->
    %% Test repo_key from get_auth_config callback
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{repo_key => <<"callback_repo_key">>}}
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Config#{trusted => true}),
    ?assertEqual(<<"callback_repo_key">>, RepoKey),
    ok.

resolve_repo_auth_trusted_auth_key_test(_Config) ->
    %% Test trusted + auth_key (no oauth_exchange) uses auth_key directly
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"auth_key_value">>}}
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(
        Config#{trusted => true, oauth_exchange => false}
    ),
    ?assertEqual(<<"auth_key_value">>, RepoKey),
    ok.

resolve_repo_auth_untrusted_ignores_auth_key_test(_Config) ->
    %% Test untrusted config ignores auth_key even when present
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"auth_key_value">>}},
        oauth_tokens => error
    }),

    Result = hex_cli_auth:resolve_repo_auth(Config#{trusted => false}),
    ?assertEqual(no_auth, Result),
    ok.

resolve_repo_auth_oauth_fallback_test(_Config) ->
    %% Test fallback to global OAuth when trusted but no auth_key
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        auth_config => #{},
        oauth_tokens =>
            {ok, #{
                access_token => <<"global_oauth">>,
                expires_at => Now + 3600
            }}
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Config#{trusted => true}),
    ?assertEqual(<<"Bearer global_oauth">>, RepoKey),
    ok.

resolve_repo_auth_no_auth_test(_Config) ->
    %% Test no_auth when untrusted and no credentials
    Config = config_with_callbacks(#{
        auth_config => #{},
        oauth_tokens => error
    }),

    Result = hex_cli_auth:resolve_repo_auth(Config#{trusted => false}),
    ?assertEqual(no_auth, Result),
    ok.

resolve_repo_auth_oauth_exchange_new_token_test(_Config) ->
    %% Test oauth_exchange with auth_key but no existing oauth_token
    Self = self(),
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"my_auth_key">>}},
        persist_oauth_tokens => fun(Scope, Access, _Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Expires},
            ok
        end
    }),

    {ok, RepoKey, AuthContext} = hex_cli_auth:resolve_repo_auth(
        Config#{trusted => true, oauth_exchange => true}
    ),
    ?assertMatch(<<"Bearer ", _/binary>>, RepoKey),
    ?assertEqual(#{source => oauth, has_refresh_token => false}, AuthContext),

    %% Verify token was persisted with repo name
    receive
        {persisted, <<"hexpm">>, _AccessToken, _ExpiresAt} -> ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

resolve_repo_auth_oauth_exchange_existing_valid_test(_Config) ->
    %% Test oauth_exchange with existing valid oauth_token reuses it
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        auth_config => #{
            <<"hexpm">> => #{
                auth_key => <<"my_auth_key">>,
                oauth_token => #{
                    access_token => <<"existing_token">>,
                    expires_at => Now + 3600
                }
            }
        }
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(
        Config#{trusted => true, oauth_exchange => true}
    ),
    ?assertEqual(<<"Bearer existing_token">>, RepoKey),
    ok.

resolve_repo_auth_oauth_exchange_existing_expired_test(_Config) ->
    %% Test oauth_exchange with expired oauth_token re-exchanges
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        auth_config => #{
            <<"hexpm">> => #{
                auth_key => <<"my_auth_key">>,
                oauth_token => #{
                    access_token => <<"expired_token">>,
                    expires_at => Now - 100
                }
            }
        },
        persist_oauth_tokens => fun(Scope, Access, _Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Expires},
            ok
        end
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(
        Config#{trusted => true, oauth_exchange => true}
    ),
    ?assertMatch(<<"Bearer ", _/binary>>, RepoKey),
    ?assertNotEqual(<<"Bearer expired_token">>, RepoKey),

    %% Verify new token was persisted
    receive
        {persisted, <<"hexpm">>, _NewAccessToken, _ExpiresAt} -> ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

resolve_repo_auth_parent_repo_auth_key_test(_Config) ->
    %% Test trusted org repo falls back to parent repo auth_key
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"parent_auth_key">>}}
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(
        Config#{repo_name => <<"hexpm:myorg">>, trusted => true, oauth_exchange => false}
    ),
    ?assertEqual(<<"parent_auth_key">>, RepoKey),
    ok.

%%====================================================================
%% Test Cases - with_api OTP handling
%%====================================================================

with_api_otp_required_test(_Config) ->
    %% Test OTP prompt when server returns otp_required
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) -> {ok, <<"123456">>} end
    }),

    CallCount = counters:new(1, []),
    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(Cfg) ->
            Count = counters:get(CallCount, 1),
            counters:add(CallCount, 1, 1),
            case Count of
                0 ->
                    %% First call: return 401 with otp_required
                    {ok,
                        {401,
                            #{
                                <<"www-authenticate">> =>
                                    <<"Bearer realm=\"hex\", error=\"totp_required\"">>
                            },
                            <<>>}};
                _ ->
                    %% Second call: should have OTP, return success
                    ?assertEqual(<<"123456">>, maps:get(api_otp, Cfg)),
                    {ok, {200, #{}, <<"success">>}}
            end
        end
    ),
    ?assertEqual({ok, {200, #{}, <<"success">>}}, Result),
    ?assertEqual(2, counters:get(CallCount, 1)),
    ok.

with_api_otp_invalid_retry_test(_Config) ->
    %% Test OTP retry when server returns invalid_totp
    Now = erlang:system_time(second),
    OtpAttempts = counters:new(1, []),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) ->
            Count = counters:get(OtpAttempts, 1),
            counters:add(OtpAttempts, 1, 1),
            case Count of
                0 -> {ok, <<"wrong_otp">>};
                _ -> {ok, <<"correct_otp">>}
            end
        end
    }),

    CallCount = counters:new(1, []),
    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(Cfg) ->
            Count = counters:get(CallCount, 1),
            counters:add(CallCount, 1, 1),
            case Count of
                0 ->
                    {ok,
                        {401,
                            #{
                                <<"www-authenticate">> =>
                                    <<"Bearer realm=\"hex\", error=\"totp_required\"">>
                            },
                            <<>>}};
                1 ->
                    ?assertEqual(<<"wrong_otp">>, maps:get(api_otp, Cfg)),
                    {ok,
                        {401,
                            #{
                                <<"www-authenticate">> =>
                                    <<"Bearer realm=\"hex\", error=\"invalid_totp\"">>
                            },
                            <<>>}};
                _ ->
                    ?assertEqual(<<"correct_otp">>, maps:get(api_otp, Cfg)),
                    {ok, {200, #{}, <<"success">>}}
            end
        end
    ),
    ?assertEqual({ok, {200, #{}, <<"success">>}}, Result),
    ?assertEqual(3, counters:get(CallCount, 1)),
    ok.

with_api_otp_cancelled_test(_Config) ->
    %% Test OTP cancellation returns error
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) -> cancelled end
    }),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(_Cfg) ->
            {ok,
                {401,
                    #{
                        <<"www-authenticate">> =>
                            <<"Bearer realm=\"hex\", error=\"totp_required\"">>
                    },
                    <<>>}}
        end
    ),
    ?assertEqual({error, {auth_error, otp_cancelled}}, Result),
    ok.

with_api_otp_max_retries_test(_Config) ->
    %% Test OTP max retries returns error
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) -> {ok, <<"wrong_otp">>} end
    }),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(_Cfg) ->
            {ok,
                {401,
                    #{<<"www-authenticate">> => <<"Bearer realm=\"hex\", error=\"invalid_totp\"">>},
                    <<>>}}
        end
    ),
    ?assertEqual({error, {auth_error, otp_max_retries}}, Result),
    ok.

%%====================================================================
%% Test Cases - with_api token refresh on 401
%%====================================================================

with_api_token_expired_refresh_test(_Config) ->
    %% Test token refresh when server returns token_expired
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"initial_token">>,
                refresh_token => <<"refresh_token">>,
                %% Within EXPIRY_BUFFER_SECONDS, will trigger refresh
                expires_at => Now + 100
            }},
        persist_oauth_tokens => fun(_Scope, Access, Refresh, Expires) ->
            Self ! {persisted, Access, Refresh, Expires},
            ok
        end
    }),

    CallCount = counters:new(1, []),
    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(Cfg) ->
            Count = counters:get(CallCount, 1),
            counters:add(CallCount, 1, 1),
            ApiKey = maps:get(api_key, Cfg),
            case Count of
                0 ->
                    %% First call gets refreshed token (initial was within expiry buffer)
                    ?assertMatch(<<"Bearer ", _/binary>>, ApiKey),
                    ?assertNotEqual(<<"Bearer initial_token">>, ApiKey),
                    {ok,
                        {401,
                            #{
                                <<"www-authenticate">> =>
                                    <<"Bearer realm=\"hex\", error=\"token_expired\"">>
                            },
                            <<>>}};
                _ ->
                    %% Second refresh after 401
                    ?assertMatch(<<"Bearer ", _/binary>>, ApiKey),
                    {ok, {200, #{}, <<"success">>}}
            end
        end
    ),
    ?assertEqual({ok, {200, #{}, <<"success">>}}, Result),
    ?assertEqual(2, counters:get(CallCount, 1)),

    %% Verify tokens were persisted (at least once for initial refresh)
    receive
        {persisted, _NewAccess, _NewRefresh, _NewExpires} -> ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

%%====================================================================
%% Test Cases - with_api reauthentication after refresh failure
%%====================================================================

with_api_token_expired_reauth_yes_test(_Config) ->
    %% When token refresh fails and user agrees to re-authenticate,
    %% device auth flow is triggered and the operation retried.
    Self = self(),
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"initial_token">>,
                %% No refresh_token; token is valid so it's used, but server
                %% returns token_expired 401, triggering the reauth path
                expires_at => Now + 3600
            }},
        should_authenticate => fun(token_refresh_failed) ->
            Self ! prompted,
            true
        end,
        persist_oauth_tokens => fun(Scope, Access, Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Refresh, Expires},
            ok
        end
    }),

    %% Queue token poll success response (device authorization response is handled
    %% automatically by hex_http_test; only the poll response needs to be queued)
    AccessToken = <<"new_device_token">>,
    RefreshToken = <<"new_refresh_token">>,
    SuccessPayload = #{
        <<"access_token">> => AccessToken,
        <<"refresh_token">> => RefreshToken,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(Cfg) ->
            ApiKey = maps:get(api_key, Cfg),
            case ApiKey of
                <<"Bearer initial_token">> ->
                    {ok,
                        {401,
                            #{
                                <<"www-authenticate">> =>
                                    <<"Bearer realm=\"hex\", error=\"token_expired\"">>
                            },
                            <<>>}};
                _ ->
                    {ok, {200, #{}, ApiKey}}
            end
        end,
        [{oauth_open_browser, false}]
    ),
    ?assertEqual({ok, {200, #{}, <<"Bearer new_device_token">>}}, Result),

    receive
        prompted -> ok
    after 100 ->
        error(should_authenticate_not_called)
    end,

    receive
        {persisted, global, AccessToken, RefreshToken, _} -> ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

with_api_token_expired_reauth_no_test(_Config) ->
    %% When token refresh fails and user declines to re-authenticate,
    %% returns auth_declined error.
    Self = self(),
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"initial_token">>,
                %% No refresh_token; token is valid so it's used, but server
                %% returns token_expired 401, triggering the reauth path
                expires_at => Now + 3600
            }},
        should_authenticate => fun(token_refresh_failed) ->
            Self ! prompted,
            false
        end
    }),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(_) ->
            {ok,
                {401,
                    #{
                        <<"www-authenticate">> =>
                            <<"Bearer realm=\"hex\", error=\"token_expired\"">>
                    },
                    <<>>}}
        end
    ),
    ?assertEqual({error, {auth_error, auth_declined}}, Result),

    receive
        prompted -> ok
    after 100 ->
        error(should_authenticate_not_called)
    end,
    ok.

with_api_token_expired_reauth_inline_false_test(_Config) ->
    %% When auth_inline is false, token refresh failure returns error directly
    %% without prompting the user.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"initial_token">>,
                %% No refresh_token; token is valid so it's used, but server
                %% returns token_expired 401, triggering the reauth path
                expires_at => Now + 3600
            }},
        should_authenticate => fun(_) -> error(should_not_be_called) end
    }),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(_) ->
            {ok,
                {401,
                    #{
                        <<"www-authenticate">> =>
                            <<"Bearer realm=\"hex\", error=\"token_expired\"">>
                    },
                    <<>>}}
        end,
        [{auth_inline, false}]
    ),
    ?assertEqual({error, {auth_error, token_refresh_failed}}, Result),
    ok.

%%====================================================================
%% Test Cases - with_api (wrapper behavior)
%%====================================================================

with_api_optional_test(_Config) ->
    %% Test optional => true allows requests without auth
    Config = config_with_callbacks(#{oauth_tokens => error}),

    %% Function is called without api_key
    Result = hex_cli_auth:with_api(
        read,
        Config,
        fun(Cfg) -> maps:get(api_key, Cfg, undefined) end,
        [{optional, true}]
    ),
    ?assertEqual(undefined, Result),
    ok.

with_api_optional_token_refresh_failed_test(_Config) ->
    %% When resolve_api_auth fails with token_refresh_failed and optional is true,
    %% fall back to executing the request without credentials.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                %% Expired with no refresh_token => token_refresh_failed immediately
                expires_at => Now - 100
            }}
    }),

    Result = hex_cli_auth:with_api(
        read,
        Config,
        fun(Cfg) -> maps:get(api_key, Cfg, undefined) end,
        [{optional, true}]
    ),
    ?assertEqual(undefined, Result),
    ok.

with_api_auth_inline_test(_Config) ->
    %% Test auth_inline => false returns error instead of prompting
    Config = config_with_callbacks(#{oauth_tokens => error}),

    Result = hex_cli_auth:with_api(
        read,
        Config,
        fun(_) -> error(should_not_be_called) end,
        [{optional, false}, {auth_inline, false}]
    ),
    ?assertEqual({error, {auth_error, no_credentials}}, Result),
    ok.

with_api_device_auth_test(_Config) ->
    %% Test device auth flow when should_authenticate returns true
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens => error,
        should_authenticate => fun(no_credentials) -> true end,
        persist_oauth_tokens => fun(Scope, Access, Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Refresh, Expires},
            ok
        end
    }),

    %% Queue success response for device auth polling
    AccessToken = <<"device_token">>,
    RefreshToken = <<"device_refresh">>,
    SuccessPayload = #{
        <<"access_token">> => AccessToken,
        <<"refresh_token">> => RefreshToken,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(Cfg) -> maps:get(api_key, Cfg) end,
        [{oauth_open_browser, false}]
    ),
    ?assertEqual(<<"Bearer device_token">>, Result),

    %% Verify token was persisted
    receive
        {persisted, global, AccessToken, RefreshToken, _} -> ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

%%====================================================================
%% Test Cases - with_repo (wrapper behavior)
%%====================================================================

with_repo_optional_test(_Config) ->
    %% Test that with_repo with optional => true (default) proceeds without auth
    Config = config_with_callbacks(#{oauth_tokens => error}),

    Result = hex_cli_auth:with_repo(
        Config#{trusted => false},
        fun(Cfg) -> maps:get(repo_key, Cfg, undefined) end
    ),
    ?assertEqual(undefined, Result),
    ok.

with_repo_trusted_with_auth_test(_Config) ->
    %% Test with_repo with trusted config and auth_key
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"my_auth_key">>}}
    }),

    Result = hex_cli_auth:with_repo(
        Config#{trusted => true, oauth_exchange => false},
        fun(Cfg) -> maps:get(repo_key, Cfg) end
    ),
    ?assertEqual(<<"my_auth_key">>, Result),
    ok.

with_repo_optional_token_refresh_failed_test(_Config) ->
    %% When resolve_repo_auth fails with token_refresh_failed and optional is true,
    %% fall back to executing the request without credentials.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                %% Expired with no refresh_token => token_refresh_failed immediately
                expires_at => Now - 100
            }}
    }),

    Result = hex_cli_auth:with_repo(
        Config#{trusted => true},
        fun(Cfg) -> maps:get(repo_key, Cfg, undefined) end
    ),
    ?assertEqual(undefined, Result),
    ok.

%%====================================================================
%% Test Cases - Concurrency
%%====================================================================

resolve_oauth_token_concurrent_refresh_serialized_test(_Config) ->
    %% Two concurrent calls to resolve_api_auth with an expired token should
    %% serialize: only one refresh happens; the second waits and then re-reads
    %% the (now-fresh) token rather than doing a second refresh.
    Now = erlang:system_time(second),
    Self = self(),
    RefreshCount = counters:new(1, [atomics]),

    %% get_oauth_tokens is called by each process; we simulate the token
    %% being updated after the first refresh by tracking call count.
    GetOAuthTokensFn = fun() ->
        Count = counters:get(RefreshCount, 1),
        case Count of
            0 ->
                %% Token is expired — both processes will see this initially
                {ok, #{
                    access_token => <<"expired_token">>,
                    refresh_token => <<"refresh_token">>,
                    expires_at => Now - 100
                }};
            _ ->
                %% After the first refresh, return a fresh token
                {ok, #{
                    access_token => <<"new_token">>,
                    refresh_token => <<"new_refresh_token">>,
                    expires_at => Now + 3600
                }}
        end
    end,

    Config = config_with_callbacks(#{
        get_oauth_tokens => GetOAuthTokensFn,
        persist_oauth_tokens => fun(_Scope, _Access, _Refresh, _Expires) ->
            %% Simulate a slow refresh so the second process must wait
            timer:sleep(100),
            counters:add(RefreshCount, 1, 1),
            Self ! refreshed,
            ok
        end
    }),

    %% Spawn two concurrent callers
    spawn(fun() ->
        Result = hex_cli_auth:resolve_api_auth(read, Config),
        Self ! {result1, Result}
    end),
    spawn(fun() ->
        Result = hex_cli_auth:resolve_api_auth(read, Config),
        Self ! {result2, Result}
    end),

    receive
        {result1, R1} -> ok
    end,
    receive
        {result2, R2} -> ok
    end,

    %% Both should succeed with a bearer token
    ?assertMatch({ok, <<"Bearer ", _/binary>>, _}, R1),
    ?assertMatch({ok, <<"Bearer ", _/binary>>, _}, R2),

    %% The token refresh should have happened exactly once
    ?assertEqual(1, counters:get(RefreshCount, 1)),

    receive
        refreshed -> ok
    after 0 -> ok
    end,
    ok.

resolve_oauth_token_refresh_failure_clears_once_test(_Config) ->
    %% Several concurrent callers share one expired global token whose refresh
    %% the server rejects (400). The first failure must invalidate the token via
    %% clear_oauth_tokens while holding the token-refresh lock; every caller
    %% serialized behind the lock then re-reads it as absent instead of each
    %% re-POSTing to /oauth/token. So the refresh is attempted exactly once.
    Now = erlang:system_time(second),
    NumCallers = 5,
    Self = self(),
    ClearCount = counters:new(1, [atomics]),

    %% Shared token store: starts with the expired token, emptied by the first
    %% (and only) clear so subsequent callers see no credentials.
    TokenStore = ets:new(token_store, [public, set]),
    true = ets:insert(
        TokenStore,
        {oauth_tokens,
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now - 100
            }}}
    ),

    Config = config_with_callbacks(#{
        get_oauth_tokens => fun() ->
            [{oauth_tokens, Tokens}] = ets:lookup(TokenStore, oauth_tokens),
            Tokens
        end,
        clear_oauth_tokens => fun() ->
            %% Slow clear: a missing lock or missing re-read would let other
            %% callers race in and refresh again, tripping the count assertion.
            timer:sleep(50),
            ets:insert(TokenStore, {oauth_tokens, error}),
            counters:add(ClearCount, 1, 1),
            ok
        end
    }),

    %% Each caller plants a 400 response for its own refresh request. Only the
    %% caller that wins the lock actually performs the refresh and consumes it.
    FailResponse =
        {ok,
            {400, #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
                term_to_binary(#{<<"error">> => <<"invalid_grant">>})}},

    [
        spawn(fun() ->
            self() ! {hex_http_test, oauth_refresh_response, FailResponse},
            Self ! {ready, self()},
            receive
                go -> ok
            end,
            Result = hex_cli_auth:resolve_api_auth(read, Config),
            Self ! {result, Result}
        end)
     || _ <- lists:seq(1, NumCallers)
    ],

    %% Barrier: release all callers together to maximize the race.
    Pids = [
        receive
            {ready, Pid} -> Pid
        after 1000 ->
            error(caller_not_ready)
        end
     || _ <- lists:seq(1, NumCallers)
    ],
    [Pid ! go || Pid <- Pids],

    Results = [
        receive
            {result, R} -> R
        after 5000 ->
            error(caller_timed_out)
        end
     || _ <- lists:seq(1, NumCallers)
    ],

    %% The token was cleared exactly once => exactly one refresh POST happened.
    ?assertEqual(1, counters:get(ClearCount, 1)),
    %% No caller obtained a usable token.
    [?assertMatch({error, _}, R) || R <- Results],

    ets:delete(TokenStore),
    ok.

device_auth_concurrent_serialized_reuses_login_test(_Config) ->
    %% Multiple concurrent callers that all need to authenticate via device auth
    %% must serialize: the FIRST caller runs the device auth flow, persists the
    %% resulting token, and every subsequent caller reuses that login instead of
    %% kicking off its own device auth flow.
    %%
    %% To make the race deterministic, all callers first sync at a barrier (so
    %% they enter `with_api` together) and the `should_authenticate' callback
    %% blocks the caller that reaches it until the test releases it. Then:
    %%   * With the global lock, only ONE caller can be inside the device auth
    %%     section at a time, so only one `should_authenticate' arrives while the
    %%     others are still blocked on the lock.
    %%   * Without the lock, ALL callers reach `should_authenticate' concurrently.
    %% We assert exactly one caller entered the prompt, then release it; the
    %% persisted token is reused by everyone else.
    NumCallers = 5,
    Self = self(),

    PromptCount = counters:new(1, [atomics]),
    PersistCount = counters:new(1, [atomics]),

    %% Persisted token store, updated by the winning device auth flow and read by
    %% every subsequent caller. Starts empty so the first caller has no credentials.
    TokenStore = ets:new(token_store, [public, set]),
    true = ets:insert(TokenStore, {oauth_tokens, error}),

    GetOAuthTokensFn = fun() ->
        [{oauth_tokens, Tokens}] = ets:lookup(TokenStore, oauth_tokens),
        Tokens
    end,

    Config = config_with_callbacks(#{
        get_oauth_tokens => GetOAuthTokensFn,
        should_authenticate => fun(no_credentials) ->
            counters:add(PromptCount, 1, 1),
            %% Signal arrival and block until the test releases us. This holds the
            %% global lock (if any), keeping other callers out of this section.
            Self ! {entered_prompt, self()},
            receive
                release -> ok
            end,
            true
        end,
        persist_oauth_tokens => fun(global, Access, Refresh, Expires) ->
            %% The winning caller persists the device token; make it valid so
            %% subsequent callers reuse it instead of authenticating again.
            ets:insert(
                TokenStore,
                {oauth_tokens,
                    {ok, #{
                        access_token => Access,
                        refresh_token => Refresh,
                        expires_at => Expires
                    }}}
            ),
            counters:add(PersistCount, 1, 1),
            ok
        end
    }),

    %% The device auth poll reads the queued oauth_device_response from the
    %% *calling* process's mailbox; each caller plants its own success response.
    AccessToken = <<"device_token">>,
    SuccessPayload = #{
        <<"access_token">> => AccessToken,
        <<"refresh_token">> => <<"device_refresh">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    DeviceResponse =
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    %% Spawn concurrent callers, all with no initial credentials. They sync at a
    %% barrier so they enter with_api as simultaneously as possible.
    [
        spawn(fun() ->
            self() ! DeviceResponse,
            Self ! {ready, self()},
            receive
                go -> ok
            end,
            Result = hex_cli_auth:with_api(
                write,
                Config,
                fun(Cfg) -> maps:get(api_key, Cfg) end,
                [{oauth_open_browser, false}]
            ),
            Self ! {result, N, Result}
        end)
     || N <- lists:seq(1, NumCallers)
    ],

    %% Barrier: wait for all callers to be ready, then release them together.
    Pids = [
        receive
            {ready, Pid} -> Pid
        after 1000 ->
            error(caller_not_ready)
        end
     || _ <- lists:seq(1, NumCallers)
    ],
    [Pid ! go || Pid <- Pids],

    %% Exactly one caller may reach the prompt; give the others time to (wrongly)
    %% reach it too if the lock is missing.
    receive
        {entered_prompt, PromptPid} ->
            %% Allow other callers to race into the (un)locked section.
            timer:sleep(200),
            ?assertEqual(
                1,
                counters:get(PromptCount, 1),
                "device auth was not serialized: multiple callers prompted concurrently"
            ),
            %% Release the winner so it completes device auth and persists.
            PromptPid ! release
    after 2000 ->
        error(no_prompt)
    end,

    %% Collect all results. Every caller must end up with the same bearer token,
    %% either because it ran the (single) device auth flow or because it reused
    %% the login persisted by the winner.
    Results = [
        receive
            {result, _N, R} -> R
        after 5000 ->
            error(caller_timed_out)
        end
     || _ <- lists:seq(1, NumCallers)
    ],

    [
        ?assertEqual(<<"Bearer device_token">>, R)
     || R <- Results
    ],

    %% The user was prompted, and the token persisted, exactly once across all
    %% callers — proving calls were serialized and the login was reused.
    ?assertEqual(1, counters:get(PromptCount, 1)),
    ?assertEqual(1, counters:get(PersistCount, 1)),

    ets:delete(TokenStore),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

config_with_callbacks(Opts) ->
    ?CONFIG#{cli_auth_callbacks => make_callbacks(Opts)}.

make_callbacks(Opts) ->
    AuthConfig = maps:get(auth_config, Opts, #{}),
    PromptOtp = maps:get(prompt_otp, Opts, fun(_) -> cancelled end),
    ShouldAuthenticate = maps:get(should_authenticate, Opts, fun(_) -> false end),
    PersistFn = maps:get(persist_oauth_tokens, Opts, fun(_, _, _, _) -> ok end),
    ClearFn = maps:get(clear_oauth_tokens, Opts, fun() -> ok end),
    DefaultGetOAuthTokens = fun() -> maps:get(oauth_tokens, Opts, error) end,
    GetOAuthTokensFn = maps:get(get_oauth_tokens, Opts, DefaultGetOAuthTokens),

    #{
        get_auth_config => fun(RepoName) -> maps:get(RepoName, AuthConfig, undefined) end,
        get_oauth_tokens => GetOAuthTokensFn,
        persist_oauth_tokens => PersistFn,
        clear_oauth_tokens => ClearFn,
        prompt_otp => PromptOtp,
        should_authenticate => ShouldAuthenticate,
        get_client_id => fun() -> <<"test_client">> end
    }.
