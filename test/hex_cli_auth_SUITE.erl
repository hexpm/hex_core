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

-define(GITHUB_OIDC_ENV_VARS, [
    "ACTIONS_ID_TOKEN_REQUEST_URL", "ACTIONS_ID_TOKEN_REQUEST_TOKEN"
]).

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
        resolve_repo_auth_oauth_fallback_child_repo_test,
        resolve_repo_auth_oauth_fallback_custom_repo_test,
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

        %% organization re-authorization
        organization_reauth_reported_on_refresh_test,
        organization_reauth_reported_empty_test,
        organization_reauth_malformed_not_reported_test,
        refresh_tokens_forces_a_refresh_test,
        refresh_tokens_without_credentials_test,

        %% with_api tests - token refresh on 401
        with_api_token_expired_refresh_test,
        with_api_token_expired_renews_test,
        with_api_token_expired_retry_bounded_test,
        with_api_token_expired_reauth_retry_bounded_test,

        %% with_api tests - reauthentication after refresh failure
        with_api_token_expired_reauth_yes_test,
        with_api_token_expired_reauth_no_test,
        with_api_token_expired_reauth_inline_false_test,

        %% with_api tests - wrapper behavior
        with_api_optional_test,
        with_api_optional_token_refresh_failed_test,
        with_api_refused_refresh_prompts_test,
        with_api_auth_inline_test,
        with_api_device_auth_test,

        %% with_repo tests - wrapper behavior
        with_repo_optional_test,
        with_repo_trusted_with_auth_test,
        with_repo_optional_token_refresh_failed_test,
        with_repo_optional_401_does_not_prompt_test,
        with_repo_device_auth_sets_repo_key_test,
        with_repo_token_expired_refresh_test,
        with_repo_token_expired_exchange_test,

        %% token refresh failure modes
        refresh_transport_error_keeps_token_test,
        refresh_server_error_keeps_token_test,
        refresh_malformed_body_keeps_token_test,
        refresh_refusal_clears_token_test,

        %% concurrency tests
        resolve_oauth_token_concurrent_refresh_serialized_test,
        resolve_oauth_token_refresh_failure_clears_once_test,
        device_auth_concurrent_serialized_reuses_login_test,
        device_auth_lock_released_before_request_test,
        resolve_repo_auth_no_credentials_skips_locks_test,
        resolve_repo_auth_valid_oauth_skips_locks_test,
        resolve_repo_auth_exchange_waits_for_lock_test,

        %% workload_identity_auth tests
        workload_identity_auth_no_provider_test,
        workload_identity_auth_credentials_present_test,
        workload_identity_auth_audience_failed_test,

        %% resolve_repo_auth tests - Workload Identity
        resolve_repo_auth_workload_identity_kept_test,
        resolve_repo_auth_workload_identity_kept_failure_test,
        resolve_repo_auth_workload_identity_not_kept_test,
        resolve_repo_auth_workload_identity_no_provider_test,
        resolve_repo_auth_workload_identity_hexpm_test,
        resolve_repo_auth_workload_identity_after_oauth_test,
        {group, oidc_token}
    ].

groups() ->
    [
        {oidc_token, [], [
            workload_identity_auth_success_test,
            workload_identity_auth_token_request_failed_test,
            workload_identity_auth_exchange_failed_test,
            resolve_repo_auth_workload_identity_test,
            resolve_repo_auth_workload_identity_expired_test,
            resolve_repo_auth_workload_identity_concurrent_test,
            resolve_repo_auth_workload_identity_concurrent_failure_test,
            with_repo_workload_identity_failed_test,
            with_repo_workload_identity_failed_unauthenticated_test,
            with_repo_workload_identity_failed_required_test,
            with_repo_token_expired_workload_identity_test,
            with_repo_token_expired_workload_identity_renewed_test,
            with_repo_token_expired_workload_identity_failed_test
        ]}
    ].

init_per_group(oidc_token, Config) ->
    case code:ensure_loaded(json) of
        {module, json} -> Config;
        {error, _Reason} -> {skip, json_unavailable}
    end.

end_per_group(oidc_token, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    lists:foreach(fun os:unsetenv/1, ?GITHUB_OIDC_ENV_VARS),
    Config.

end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun os:unsetenv/1, ?GITHUB_OIDC_ENV_VARS),
    ok.

%%====================================================================
%% Test Cases - resolve_api_auth
%%====================================================================

resolve_api_auth_config_passthrough_test(_Config) ->
    %% When api_key is already in config, it should be used directly
    Config = config_with_callbacks(#{}),
    ConfigWithKey = Config#{api_key => <<"config_api_key">>},

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(read, ConfigWithKey),
    ?assertEqual(<<"config_api_key">>, ApiKey),
    ?assertEqual(#{has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_per_repo_test(_Config) ->
    %% Test per-repo api_key from callback
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{api_key => <<"repo_api_key">>}}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(write, Config),
    ?assertEqual(<<"repo_api_key">>, ApiKey),
    ?assertEqual(#{has_refresh_token => false}, AuthContext),
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
    ?assertEqual(#{has_refresh_token => true}, AuthContext),
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
    ?assertEqual(#{has_refresh_token => true}, AuthContext),

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
    ?assertEqual(#{has_refresh_token => false}, AuthContext),
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
    ?assertEqual(#{has_refresh_token => false}, AuthContext),
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

resolve_repo_auth_oauth_fallback_child_repo_test(_Config) ->
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        auth_config => #{},
        oauth_tokens =>
            {ok, #{
                access_token => <<"global_oauth">>,
                expires_at => Now + 3600
            }}
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(
        Config#{repo_organization => <<"myorg">>, trusted => true}
    ),
    ?assertEqual(<<"Bearer global_oauth">>, RepoKey),
    ok.

resolve_repo_auth_oauth_fallback_custom_repo_test(_Config) ->
    Config = config_with_callbacks(#{
        auth_config => #{},
        get_oauth_tokens => fun() -> error(global_oauth_callback_called) end
    }),

    Result = hex_cli_auth:resolve_repo_auth(
        Config#{repo_name => <<"custom">>, trusted => true}
    ),
    ?assertEqual(no_auth, Result),
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
    ?assertEqual(#{has_refresh_token => false}, AuthContext),

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

with_api_token_expired_renews_test(_Config) ->
    %% An API token the server rejects as expired is refreshed even though its
    %% stored expiry has not passed, the way a repository token is, and the
    %% request runs again with the token that came back.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"stale_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }}
    }),

    queue_refresh_response(#{<<"access_token">> => <<"renewed_token">>}),

    Fun = fun(Cfg) ->
        case maps:get(api_key, Cfg) of
            <<"Bearer stale_token">> -> token_expired_response();
            ApiKey -> {ok, {200, #{}, ApiKey}}
        end
    end,

    ?assertEqual(
        {ok, {200, #{}, <<"Bearer renewed_token">>}},
        hex_cli_auth:with_api(write, Config, Fun)
    ),
    ok.

with_api_token_expired_retry_bounded_test(_Config) ->
    %% A server that answers token_expired to every bearer it is sent gets a
    %% bounded number of requests: the renewed token is tried once and the 401
    %% is handed back, rather than renewed and retried without end.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"stale_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }},
        should_authenticate => fun(_Reason) -> error(should_not_be_called) end
    }),

    CallCount = counters:new(1, []),
    Result = hex_cli_auth:with_api(write, Config, fun(_Cfg) ->
        counters:add(CallCount, 1, 1),
        token_expired_response()
    end),

    ?assertMatch({ok, {401, _Headers, _Body}}, Result),
    ?assertEqual(2, counters:get(CallCount, 1)),
    ok.

with_api_token_expired_reauth_retry_bounded_test(_Config) ->
    %% Same bound when the renewal is a device auth: the user authenticates
    %% once, the request runs again with the new token, and a second
    %% token_expired is the caller's to handle rather than a second prompt.
    Now = erlang:system_time(second),
    PromptCount = counters:new(1, []),
    CallCount = counters:new(1, []),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"initial_token">>,
                %% No refresh_token, so the 401 goes straight to reauth
                expires_at => Now + 3600
            }},
        should_authenticate => fun(token_refresh_failed) ->
            counters:add(PromptCount, 1, 1),
            true
        end
    }),

    queue_device_response(<<"device_token">>),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(_Cfg) ->
            counters:add(CallCount, 1, 1),
            token_expired_response()
        end,
        [{oauth_open_browser, false}]
    ),

    ?assertMatch({ok, {401, _Headers, _Body}}, Result),
    ?assertEqual(1, counters:get(PromptCount, 1)),
    ?assertEqual(2, counters:get(CallCount, 1)),
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

with_api_refused_refresh_prompts_test(_Config) ->
    %% A refused refresh leaves no usable token, same as having none, so the
    %% caller that asked to be prompted up front is asked rather than told to
    %% run mix hex.user auth.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                expires_at => Now - 100
            }},
        should_authenticate => fun(Reason) ->
            Self ! {should_authenticate, Reason},
            false
        end
    }),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(_) -> error(should_not_be_called) end,
        [{optional, false}, {auth_inline, true}]
    ),

    receive
        {should_authenticate, Reason} ->
            ?assertEqual(token_refresh_failed, Reason)
    after 0 ->
        ct:fail("should_authenticate was never called")
    end,

    ?assertEqual({error, {auth_error, auth_declined}}, Result),
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

with_repo_optional_401_does_not_prompt_test(_Config) ->
    %% with_repo defaults auth_inline to false, so a 401 on a private package
    %% returns instead of opening a device auth flow the caller did not ask for.
    Self = self(),
    Config = config_with_callbacks(#{
        should_authenticate => fun(_Reason) ->
            Self ! prompted,
            false
        end
    }),

    Result = hex_cli_auth:with_repo(
        Config#{trusted => true},
        fun(_Cfg) -> {ok, {401, #{}, <<"">>}} end
    ),
    ?assertEqual({error, {auth_error, no_credentials}}, Result),

    receive
        prompted -> error(prompted_without_auth_inline)
    after 0 -> ok
    end,
    ok.

with_repo_device_auth_sets_repo_key_test(_Config) ->
    %% Authenticating inline from a repository request must retry with
    %% repository auth: hex_repo only reads repo_key, so an api_key-shaped retry
    %% goes out with no authorization header at all.
    TokenStore = ets:new(token_store, [public, set]),
    true = ets:insert(TokenStore, {oauth_tokens, error}),

    Config = config_with_callbacks(#{
        get_oauth_tokens => fun() ->
            [{oauth_tokens, Tokens}] = ets:lookup(TokenStore, oauth_tokens),
            Tokens
        end,
        should_authenticate => fun(no_credentials) -> true end,
        persist_oauth_tokens => fun(global, Access, Refresh, Expires) ->
            ets:insert(
                TokenStore,
                {oauth_tokens,
                    {ok, #{
                        access_token => Access,
                        refresh_token => Refresh,
                        expires_at => Expires
                    }}}
            ),
            ok
        end
    }),

    queue_device_response(<<"device_token">>),

    Fun = fun(Cfg) ->
        case maps:get(repo_key, Cfg, undefined) of
            undefined -> {ok, {401, #{}, <<"">>}};
            RepoKey -> RepoKey
        end
    end,

    Result = hex_cli_auth:with_repo(
        Config#{trusted => true},
        Fun,
        [{auth_inline, true}, {oauth_open_browser, false}]
    ),
    ?assertEqual(<<"Bearer device_token">>, Result),

    ets:delete(TokenStore),
    ok.

with_repo_token_expired_refresh_test(_Config) ->
    %% A repository token the server rejects as expired is refreshed and the
    %% request runs again, rather than the 401 reaching the caller.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"stale_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }}
    }),

    queue_refresh_response(#{<<"access_token">> => <<"renewed_token">>}),

    Fun = fun(Cfg) ->
        case maps:get(repo_key, Cfg) of
            <<"Bearer stale_token">> -> token_expired_response();
            RepoKey -> RepoKey
        end
    end,

    Result = hex_cli_auth:with_repo(Config#{trusted => true}, Fun),
    ?assertEqual(<<"Bearer renewed_token">>, Result),
    ok.

with_repo_token_expired_exchange_test(_Config) ->
    %% Same for a per-repo token: it is exchanged again from the auth_key it
    %% came from, even though its stored expiry has not passed.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        auth_config => #{
            <<"hexpm">> => #{
                auth_key => <<"repo_auth_key">>,
                oauth_token => #{
                    access_token => <<"stale_repo_token">>,
                    expires_at => Now + 3600
                }
            }
        },
        persist_oauth_tokens => fun(Scope, Access, Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Refresh, Expires},
            ok
        end
    }),

    Fun = fun(Cfg) ->
        case maps:get(repo_key, Cfg) of
            <<"Bearer stale_repo_token">> -> token_expired_response();
            RepoKey -> RepoKey
        end
    end,

    Result = hex_cli_auth:with_repo(Config#{trusted => true, oauth_exchange => true}, Fun),
    ?assertMatch(<<"Bearer ", _/binary>>, Result),
    ?assertNotEqual(<<"Bearer stale_repo_token">>, Result),

    receive
        {persisted, <<"hexpm">>, _Access, RefreshToken, _Expires} ->
            ?assertEqual(undefined, RefreshToken)
    after 100 ->
        error(token_not_exchanged)
    end,
    ok.

refresh_transport_error_keeps_token_test(_Config) ->
    %% A refresh that never reached the server says nothing about the stored
    %% token, so it is kept and the caller is told the difference.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now - 100
            }},
        clear_oauth_tokens => fun() ->
            Self ! cleared,
            ok
        end
    }),

    Self ! {hex_http_test, oauth_refresh_response, {error, timeout}},

    ?assertEqual(
        {error, {auth_error, token_refresh_unavailable}},
        hex_cli_auth:resolve_api_auth(read, Config)
    ),

    receive
        cleared -> error(token_cleared_on_transport_error)
    after 0 -> ok
    end,
    ok.

refresh_server_error_keeps_token_test(_Config) ->
    %% A 429 or a 5xx is the server having a bad minute, not a refusal of the
    %% refresh token, so the stored token survives it.
    Statuses = [429, 500, 502, 503],

    [
        begin
            Self = self(),
            Config = refresh_failure_config(Self),
            Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
            Self !
                {hex_http_test, oauth_refresh_response,
                    {ok, {Status, Headers, term_to_binary(#{<<"error">> => <<"server_error">>})}}},

            ?assertEqual(
                {error, {auth_error, token_refresh_unavailable}},
                hex_cli_auth:resolve_api_auth(read, Config)
            ),

            receive
                cleared -> error({token_cleared_on_server_error, Status})
            after 0 -> ok
            end
        end
     || Status <- Statuses
    ],
    ok.

refresh_malformed_body_keeps_token_test(_Config) ->
    %% A 200 whose body is not a token response says nothing about the refresh
    %% token either, so it is not read as the server refusing it.
    Bodies = [
        <<"not a map">>,
        #{<<"expires_in">> => 3600},
        #{<<"access_token">> => <<"new_access_token">>},
        #{<<"access_token">> => <<"new_access_token">>, <<"expires_in">> => <<"3600">>},
        #{<<"access_token">> => 42, <<"expires_in">> => 3600}
    ],

    [
        begin
            Self = self(),
            Config = refresh_failure_config(Self),
            Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
            Self !
                {hex_http_test, oauth_refresh_response, {ok, {200, Headers, term_to_binary(Body)}}},

            ?assertEqual(
                {error, {auth_error, token_refresh_unavailable}},
                hex_cli_auth:resolve_api_auth(read, Config)
            ),

            receive
                cleared -> error({token_cleared_on_malformed_body, Body})
            after 0 -> ok
            end
        end
     || Body <- Bodies
    ],
    ok.

refresh_refusal_clears_token_test(_Config) ->
    %% A 400 or a 401 is the server refusing the refresh token itself: it will
    %% not work again, so the stored token is dropped.
    Statuses = [400, 401],

    [
        begin
            Self = self(),
            Config = refresh_failure_config(Self),
            Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
            Self !
                {hex_http_test, oauth_refresh_response,
                    {ok, {Status, Headers, term_to_binary(#{<<"error">> => <<"invalid_grant">>})}}},

            ?assertEqual(
                {error, {auth_error, token_refresh_failed}},
                hex_cli_auth:resolve_api_auth(read, Config)
            ),

            receive
                cleared -> ok
            after 100 -> error({token_not_cleared, Status})
            end
        end
     || Status <- Statuses
    ],
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

device_auth_lock_released_before_request_test(_Config) ->
    %% The device auth lock covers acquiring the credential, not running the
    %% request. A request answering 401 comes back to the same lock, and
    %% global:trans/4 on a lock this process already holds does not nest: the
    %% inner transaction releases it while the outer one is still running.
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens => error,
        should_authenticate => fun(no_credentials) ->
            Self ! {locked_during_prompt, device_auth_lock_held()},
            true
        end
    }),

    queue_device_response(<<"device_token">>),

    Result = hex_cli_auth:with_api(
        write,
        Config,
        fun(Cfg) ->
            Self ! {locked_during_request, device_auth_lock_held()},
            maps:get(api_key, Cfg)
        end,
        [{oauth_open_browser, false}]
    ),
    ?assertEqual(<<"Bearer device_token">>, Result),

    receive
        {locked_during_prompt, LockedDuringPrompt} ->
            ?assertEqual(true, LockedDuringPrompt)
    after 100 ->
        error(should_authenticate_not_called)
    end,

    receive
        {locked_during_request, LockedDuringRequest} ->
            ?assertEqual(false, LockedDuringRequest)
    after 100 ->
        error(request_not_run)
    end,
    ok.

resolve_repo_auth_no_credentials_skips_locks_test(_Config) ->
    %% Resolving to no credentials exchanges and refreshes nothing, so it must
    %% not wait on locks held by another caller.
    Config = config_with_callbacks(#{oauth_tokens => error}),
    Holder = hold_locks([
        {hex_cli_auth, repo, <<"hexpm">>},
        {hex_cli_auth, token_refresh}
    ]),

    ?assertEqual(no_auth, resolve_repo_auth_within(Config#{trusted => true}, 1000)),
    release_locks(Holder),
    ok.

resolve_repo_auth_valid_oauth_skips_locks_test(_Config) ->
    %% A stored global token that is still valid is used without waiting on the
    %% repo or token refresh locks.
    Now = erlang:system_time(second),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"global_oauth">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }}
    }),
    Holder = hold_locks([
        {hex_cli_auth, repo, <<"hexpm">>},
        {hex_cli_auth, token_refresh}
    ]),

    ?assertEqual(
        {ok, <<"Bearer global_oauth">>, #{has_refresh_token => true}},
        resolve_repo_auth_within(Config#{trusted => true}, 1000)
    ),
    release_locks(Holder),
    ok.

resolve_repo_auth_exchange_waits_for_lock_test(_Config) ->
    %% Exchanging an auth_key for a repository token still holds the repo lock,
    %% so concurrent callers don't each exchange.
    Config = config_with_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"my_auth_key">>}}
    }),
    Holder = hold_locks([{hex_cli_auth, repo, <<"hexpm">>}]),
    Self = self(),
    spawn_link(fun() ->
        Self !
            {resolved,
                hex_cli_auth:resolve_repo_auth(Config#{trusted => true, oauth_exchange => true})}
    end),

    receive
        {resolved, Early} -> error({resolved_while_locked, Early})
    after 200 ->
        ok
    end,

    release_locks(Holder),
    receive
        {resolved, Result} ->
            ?assertMatch({ok, <<"Bearer ", _/binary>>, #{has_refresh_token := false}}, Result)
    after 5000 ->
        error(not_resolved_after_release)
    end,
    ok.

%%====================================================================
%% Test Cases - workload_identity_auth
%%====================================================================

workload_identity_auth_no_provider_test(_Config) ->
    Config = config_with_callbacks(#{}),
    ?assertEqual(none, hex_cli_auth:workload_identity_auth(Config, <<"package:hexpm/foo">>)),
    ok.

workload_identity_auth_credentials_present_test(_Config) ->
    %% A URL that no fixture answers, so the test crashes if trusted
    %% publishing tries to use it: a configured api_key takes precedence and
    %% is found before the CI provider is even looked at.
    put_github_oidc_env("https://ci.test/unreachable"),
    Config = (config_with_callbacks(#{}))#{api_key => <<"configured_api_key">>},
    ?assertEqual(none, hex_cli_auth:workload_identity_auth(Config, <<"package:hexpm/foo">>)),
    ok.

workload_identity_auth_success_test(_Config) ->
    put_github_oidc_env("https://ci.test/token"),
    Config = config_with_callbacks(#{}),

    queue_oidc_audience_response(#{<<"audience">> => <<"hexpm">>}),
    queue_ci_token_response({ok, {200, #{}, <<"{\"count\":1,\"value\":\"the.oidc.token\"}">>}}),
    queue_jwt_bearer_response(#{<<"access_token">> => <<"minted_token">>}),

    ?assertEqual(
        {ok, <<"Bearer minted_token">>},
        hex_cli_auth:workload_identity_auth(Config, <<"package:hexpm/foo">>)
    ),
    ok.

workload_identity_auth_audience_failed_test(_Config) ->
    put_github_oidc_env("https://ci.test/token"),
    Config = config_with_callbacks(#{}),

    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Body = #{<<"status">> => 404, <<"message">> => <<"Not found">>},
    self() !
        {hex_http_test, oidc_audience_response, {ok, {404, Headers, term_to_binary(Body)}}},

    ?assertEqual(
        {error, {oidc_audience_failed, {ok, {404, Headers, Body}}}},
        hex_cli_auth:workload_identity_auth(Config, <<"package:hexpm/foo">>)
    ),
    ok.

workload_identity_auth_token_request_failed_test(_Config) ->
    put_github_oidc_env("https://ci.test/token"),
    Config = config_with_callbacks(#{}),

    queue_oidc_audience_response(#{<<"audience">> => <<"hexpm">>}),
    queue_ci_token_response({ok, {403, #{}, <<"">>}}),

    ?assertEqual(
        {error, {oidc_token_request_failed, 403}},
        hex_cli_auth:workload_identity_auth(Config, <<"package:hexpm/foo">>)
    ),
    ok.

workload_identity_auth_exchange_failed_test(_Config) ->
    put_github_oidc_env("https://ci.test/token"),
    Config = config_with_callbacks(#{}),

    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Body = #{<<"error">> => <<"access_denied">>, <<"error_description">> => <<"No match">>},

    queue_oidc_audience_response(#{<<"audience">> => <<"hexpm">>}),
    queue_ci_token_response({ok, {200, #{}, <<"{\"count\":1,\"value\":\"the.oidc.token\"}">>}}),
    self() ! {hex_http_test, jwt_bearer_response, {ok, {403, Headers, term_to_binary(Body)}}},

    ?assertEqual(
        {error, {token_exchange_failed, {ok, {403, Headers, Body}}}},
        hex_cli_auth:workload_identity_auth(Config, <<"package:hexpm/foo">>)
    ),
    ok.

%%====================================================================
%% Test Cases - resolve_repo_auth with Workload Identity
%%====================================================================

resolve_repo_auth_workload_identity_test(_Config) ->
    %% An organization repository with no other credentials exchanges the CI
    %% job's OIDC token for a token scoped to the organization's repository,
    %% and hands it to the build tool to keep.
    put_github_oidc_env("https://ci.test/token"),
    Config = workload_identity_config(self(), error),
    Before = erlang:system_time(second),

    ?assertEqual(
        {ok, <<"Bearer minted.repository:acme">>, #{has_refresh_token => false}},
        hex_cli_auth:resolve_repo_auth(Config)
    ),

    receive
        {workload_identity_persisted, <<"hexpm:acme">>,
            {ok, #{access_token := AccessToken, expires_at := ExpiresAt}}} ->
            ?assertEqual(<<"minted.repository:acme">>, AccessToken),
            ?assert(ExpiresAt >= Before + 900)
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

resolve_repo_auth_workload_identity_kept_test(_Config) ->
    %% A kept token that is still valid is used without exchanging and without
    %% waiting on the repo lock. No fixture answers the CI token URL, so the
    %% test crashes if an OIDC token is requested.
    put_github_oidc_env("https://ci.test/unreachable"),
    Now = erlang:system_time(second),
    Kept = {ok, #{access_token => <<"kept_token">>, expires_at => Now + 900}},
    Config = workload_identity_config(self(), Kept),
    Holder = hold_locks([{hex_cli_auth, repo, <<"hexpm:acme">>}]),

    ?assertEqual(
        {ok, <<"Bearer kept_token">>, #{has_refresh_token => false}},
        resolve_repo_auth_within(Config, 1000)
    ),
    release_locks(Holder),
    ok.

resolve_repo_auth_workload_identity_kept_failure_test(_Config) ->
    %% A failed exchange is kept and returned without exchanging again, since
    %% every failed exchange counts against the CI job's limit at Hex. No
    %% fixture answers the CI token URL, so the test crashes if an OIDC token is
    %% requested.
    put_github_oidc_env("https://ci.test/unreachable"),
    Config = workload_identity_config(self(), {error, {oidc_token_request_failed, 403}}),

    ?assertEqual(
        {error, {auth_error, {workload_identity_failed, {oidc_token_request_failed, 403}}}},
        hex_cli_auth:resolve_repo_auth(Config)
    ),
    ok.

resolve_repo_auth_workload_identity_expired_test(_Config) ->
    %% A kept token that is about to expire is exchanged again.
    put_github_oidc_env("https://ci.test/token"),
    Now = erlang:system_time(second),
    Kept = {ok, #{access_token => <<"old_token">>, expires_at => Now + 60}},
    Config = workload_identity_config(self(), Kept),

    ?assertEqual(
        {ok, <<"Bearer minted.repository:acme">>, #{has_refresh_token => false}},
        hex_cli_auth:resolve_repo_auth(Config)
    ),

    receive
        {workload_identity_persisted, <<"hexpm:acme">>,
            {ok, #{access_token := <<"minted.repository:acme">>}}} ->
            ok
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

resolve_repo_auth_workload_identity_concurrent_test(_Config) ->
    %% Exchanging holds the repo lock, and the callers that waited for it find
    %% the kept token, so concurrent requests to one organization share a
    %% single exchange.
    put_github_oidc_env("https://ci.test/token"),
    Self = self(),
    Store = ets:new(workload_identity_tokens, [public]),
    Config = workload_identity_store_config(Self, Store),
    Holder = hold_locks([{hex_cli_auth, repo, <<"hexpm:acme">>}]),

    [
        spawn_link(fun() -> Self ! {resolved, hex_cli_auth:resolve_repo_auth(Config)} end)
     || _ <- lists:seq(1, 3)
    ],

    receive
        {resolved, Early} -> error({resolved_while_locked, Early})
    after 200 ->
        ok
    end,

    release_locks(Holder),
    [
        receive
            {resolved, Result} ->
                ?assertEqual(
                    {ok, <<"Bearer minted.repository:acme">>, #{has_refresh_token => false}},
                    Result
                )
        after 5000 ->
            error(not_resolved_after_release)
        end
     || _ <- lists:seq(1, 3)
    ],

    receive
        {workload_identity_persisted, <<"hexpm:acme">>, {ok, _Token}} -> ok
    after 0 ->
        error(token_not_persisted)
    end,
    receive
        {workload_identity_persisted, _, _} = Again -> error({exchanged_again, Again})
    after 0 ->
        ok
    end,
    ets:delete(Store),
    ok.

resolve_repo_auth_workload_identity_concurrent_failure_test(_Config) ->
    %% The callers that waited for the repo lock find the failed exchange kept
    %% and return it, so a refusal is exchanged once rather than once per
    %% request.
    put_github_oidc_env("https://ci.test/token"),
    Self = self(),
    Store = ets:new(workload_identity_tokens, [public]),
    Config = workload_identity_store_config(Self, Store),
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Body = #{<<"error">> => <<"access_denied">>, <<"error_description">> => <<"No match">>},
    Refusal = {ok, {403, Headers, term_to_binary(Body)}},
    Holder = hold_locks([{hex_cli_auth, repo, <<"hexpm:acme">>}]),

    [
        spawn_link(fun() ->
            self() ! {hex_http_test, jwt_bearer_response, Refusal},
            Self ! {resolved, hex_cli_auth:resolve_repo_auth(Config)}
        end)
     || _ <- lists:seq(1, 3)
    ],

    receive
        {resolved, Early} -> error({resolved_while_locked, Early})
    after 200 ->
        ok
    end,

    release_locks(Holder),
    Expected =
        {error,
            {auth_error,
                {workload_identity_failed, {token_exchange_failed, {ok, {403, Headers, Body}}}}}},
    [
        receive
            {resolved, Result} -> ?assertEqual(Expected, Result)
        after 5000 ->
            error(not_resolved_after_release)
        end
     || _ <- lists:seq(1, 3)
    ],

    receive
        {workload_identity_persisted, <<"hexpm:acme">>, {error, _Reason}} -> ok
    after 0 ->
        error(failure_not_persisted)
    end,
    receive
        {workload_identity_persisted, _, _} = Again -> error({exchanged_again, Again})
    after 0 ->
        ok
    end,
    ets:delete(Store),
    ok.

resolve_repo_auth_workload_identity_not_kept_test(_Config) ->
    %% A build tool without the Workload Identity callbacks would have every
    %% request exchange a new OIDC token, so repositories don't use it.
    put_github_oidc_env("https://ci.test/unreachable"),
    Config = config_with_callbacks(#{oauth_tokens => error}),

    ?assertEqual(
        no_auth,
        hex_cli_auth:resolve_repo_auth(Config#{repo_organization => <<"acme">>, trusted => true})
    ),
    ok.

resolve_repo_auth_workload_identity_no_provider_test(_Config) ->
    Config = workload_identity_config(self(), error),
    ?assertEqual(no_auth, hex_cli_auth:resolve_repo_auth(Config)),
    ok.

resolve_repo_auth_workload_identity_hexpm_test(_Config) ->
    %% The public repository needs no credentials, so nothing is exchanged for
    %% it.
    put_github_oidc_env("https://ci.test/unreachable"),
    Config = workload_identity_config(self(), error),

    ?assertEqual(
        no_auth, hex_cli_auth:resolve_repo_auth(maps:remove(repo_organization, Config))
    ),
    ok.

resolve_repo_auth_workload_identity_after_oauth_test(_Config) ->
    %% A user's own credentials take precedence over the CI job's.
    put_github_oidc_env("https://ci.test/unreachable"),
    Now = erlang:system_time(second),
    Config = workload_identity_config(self(), error),
    Callbacks = maps:get(cli_auth_callbacks, Config),
    GetOAuthTokens = fun() ->
        {ok, #{access_token => <<"global_oauth">>, expires_at => Now + 3600}}
    end,

    ?assertEqual(
        {ok, <<"Bearer global_oauth">>, #{has_refresh_token => false}},
        hex_cli_auth:resolve_repo_auth(
            Config#{cli_auth_callbacks => Callbacks#{get_oauth_tokens => GetOAuthTokens}}
        )
    ),
    ok.

with_repo_workload_identity_failed_test(_Config) ->
    %% After a refused exchange the request runs without credentials, and the
    %% repository refusing it is answered with why the exchange failed.
    put_github_oidc_env("https://ci.test/token"),
    Config = workload_identity_config(self(), error),
    {Headers, Body} = queue_jwt_bearer_refusal(),

    Fun = fun(RequestConfig) ->
        ?assertEqual(undefined, maps:get(repo_key, RequestConfig, undefined)),
        {ok, {401, #{}, <<"">>}}
    end,

    ?assertEqual(
        {error,
            {auth_error,
                {workload_identity_failed, {token_exchange_failed, {ok, {403, Headers, Body}}}}}},
        hex_cli_auth:with_repo(Config, Fun)
    ),

    receive
        {workload_identity_persisted, <<"hexpm:acme">>, {error, {token_exchange_failed, _}}} -> ok
    after 100 ->
        error(failure_not_persisted)
    end,
    ok.

with_repo_workload_identity_failed_unauthenticated_test(_Config) ->
    %% The workload identity was only picked up from the CI job, so a mirror
    %% that authenticates another way still answers the request it would have
    %% got outside CI.
    put_github_oidc_env("https://ci.test/token"),
    Config = workload_identity_config(self(), error),
    queue_jwt_bearer_refusal(),

    Fun = fun(RequestConfig) ->
        ?assertEqual(undefined, maps:get(repo_key, RequestConfig, undefined)),
        {ok, {200, #{}, <<"body">>}}
    end,

    ?assertEqual({ok, {200, #{}, <<"body">>}}, hex_cli_auth:with_repo(Config, Fun)),
    ok.

with_repo_workload_identity_failed_required_test(_Config) ->
    put_github_oidc_env("https://ci.test/token"),
    Config = workload_identity_config(self(), error),
    {Headers, Body} = queue_jwt_bearer_refusal(),

    ?assertEqual(
        {error,
            {auth_error,
                {workload_identity_failed, {token_exchange_failed, {ok, {403, Headers, Body}}}}}},
        hex_cli_auth:with_repo(
            Config, fun(_RequestConfig) -> error(request_made) end, [{optional, false}]
        )
    ),
    ok.

with_repo_token_expired_workload_identity_test(_Config) ->
    %% A kept token the repository answers token_expired for is exchanged again
    %% even though its expiry has not passed.
    put_github_oidc_env("https://ci.test/token"),
    Now = erlang:system_time(second),
    Kept = {ok, #{access_token => <<"stale_token">>, expires_at => Now + 900}},
    Config = workload_identity_config(self(), Kept),

    Fun = fun(Cfg) ->
        case maps:get(repo_key, Cfg) of
            <<"Bearer stale_token">> -> token_expired_response();
            RepoKey -> RepoKey
        end
    end,

    ?assertEqual(<<"Bearer minted.repository:acme">>, hex_cli_auth:with_repo(Config, Fun)),
    ok.

with_repo_token_expired_workload_identity_renewed_test(_Config) ->
    %% A token another request already renewed is used instead of exchanging
    %% again. No fixture answers the CI token URL, so the test crashes if an
    %% OIDC token is requested.
    put_github_oidc_env("https://ci.test/unreachable"),
    Now = erlang:system_time(second),
    Reads = counters:new(1, []),
    Config = workload_identity_config(self(), fun() ->
        counters:add(Reads, 1, 1),
        case counters:get(Reads, 1) of
            1 -> {ok, #{access_token => <<"stale_token">>, expires_at => Now + 900}};
            _ -> {ok, #{access_token => <<"renewed_token">>, expires_at => Now + 900}}
        end
    end),

    Fun = fun(Cfg) ->
        case maps:get(repo_key, Cfg) of
            <<"Bearer stale_token">> -> token_expired_response();
            RepoKey -> RepoKey
        end
    end,

    ?assertEqual(<<"Bearer renewed_token">>, hex_cli_auth:with_repo(Config, Fun)),
    ok.

with_repo_token_expired_workload_identity_failed_test(_Config) ->
    %% The request needed the token it was renewing, so a refused renewal is
    %% returned instead of the 401 that asked for it.
    put_github_oidc_env("https://ci.test/token"),
    Now = erlang:system_time(second),
    Kept = {ok, #{access_token => <<"stale_token">>, expires_at => Now + 900}},
    Config = workload_identity_config(self(), Kept),
    {Headers, Body} = queue_jwt_bearer_refusal(),

    Fun = fun(Cfg) ->
        <<"Bearer stale_token">> = maps:get(repo_key, Cfg),
        token_expired_response()
    end,

    ?assertEqual(
        {error,
            {auth_error,
                {workload_identity_failed, {token_exchange_failed, {ok, {403, Headers, Body}}}}}},
        hex_cli_auth:with_repo(Config, Fun)
    ),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

hold_locks(ResourceIds) ->
    Parent = self(),
    Holder = spawn_link(fun() ->
        [true = global:set_lock({Id, self()}, [node()], 0) || Id <- ResourceIds],
        Parent ! {locks_held, self()},
        receive
            release -> [global:del_lock({Id, self()}, [node()]) || Id <- ResourceIds]
        end,
        Parent ! {locks_released, self()}
    end),
    receive
        {locks_held, Holder} -> Holder
    after 5000 ->
        error(locks_not_acquired)
    end.

release_locks(Holder) ->
    Holder ! release,
    receive
        {locks_released, Holder} -> ok
    after 5000 ->
        error(locks_not_released)
    end.

resolve_repo_auth_within(Config, Timeout) ->
    Self = self(),
    spawn_link(fun() -> Self ! {resolved, hex_cli_auth:resolve_repo_auth(Config)} end),
    receive
        {resolved, Result} -> Result
    after Timeout ->
        error(resolve_repo_auth_blocked)
    end.

organization_reauth_reported_on_refresh_test(_Config) ->
    %% The organizations the server flags on a refresh reach the build tool.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now - 100
            }},
        organization_reauth => fun(Organizations) ->
            Self ! {organization_reauth, Organizations},
            ok
        end
    }),

    queue_refresh_response(#{
        <<"organization_reauth_required">> => [
            #{<<"organization">> => <<"acme">>, <<"requirements">> => [<<"sso">>]}
        ]
    }),

    {ok, _ApiKey, _AuthContext} = hex_cli_auth:resolve_api_auth(read, Config),

    receive
        {organization_reauth, Organizations} ->
            ?assertEqual(
                [#{organization => <<"acme">>, requirements => [<<"sso">>]}], Organizations
            )
    after 100 ->
        error(organization_reauth_not_called)
    end,
    ok.

organization_reauth_reported_empty_test(_Config) ->
    %% A server that says nothing means nothing is lapsed, and the build tool
    %% is told so rather than left holding a stale set.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now - 100
            }},
        organization_reauth => fun(Organizations) ->
            Self ! {organization_reauth, Organizations},
            ok
        end
    }),

    {ok, _ApiKey, _AuthContext} = hex_cli_auth:resolve_api_auth(read, Config),

    receive
        {organization_reauth, Organizations} -> ?assertEqual([], Organizations)
    after 100 ->
        error(organization_reauth_not_called)
    end,
    ok.

organization_reauth_malformed_not_reported_test(_Config) ->
    %% A set the server sent in a shape we cannot read is not reported at all.
    %% The build tool takes the empty list for "nothing lapsed" and deletes the
    %% organizations it holds, which drops the prompt the user needs.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now - 100
            }},
        organization_reauth => fun(Organizations) ->
            Self ! {organization_reauth, Organizations},
            ok
        end
    }),

    queue_refresh_response(#{<<"organization_reauth_required">> => <<"acme">>}),

    {ok, _ApiKey, _AuthContext} = hex_cli_auth:resolve_api_auth(read, Config),

    receive
        {organization_reauth, Organizations} -> error({organization_reauth_reported, Organizations})
    after 0 -> ok
    end,
    ok.

refresh_tokens_forces_a_refresh_test(_Config) ->
    %% A token that has not expired is still refreshed: what it carries can
    %% change without its lifetime running out.
    Now = erlang:system_time(second),
    Self = self(),
    Config = config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"valid_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }},
        persist_oauth_tokens => fun(Scope, Access, Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Refresh, Expires},
            ok
        end
    }),

    queue_refresh_response(#{<<"access_token">> => <<"renewed_token">>}),

    ?assertEqual(ok, hex_cli_auth:refresh_tokens(Config)),

    receive
        {persisted, global, Access, _Refresh, _Expires} ->
            ?assertEqual(<<"renewed_token">>, Access)
    after 100 ->
        error(token_not_persisted)
    end,
    ok.

refresh_tokens_without_credentials_test(_Config) ->
    Config = config_with_callbacks(#{}),

    ?assertEqual(
        {error, {auth_error, no_credentials}},
        hex_cli_auth:refresh_tokens(Config)
    ),
    ok.

%% @private
%% Plants the next refresh response the test HTTP adapter will hand back,
%% merged over a working one so a test only states what it cares about.
queue_refresh_response(Overrides) ->
    Payload = maps:merge(
        #{
            <<"access_token">> => <<"new_access_token">>,
            <<"refresh_token">> => <<"new_refresh_token">>,
            <<"token_type">> => <<"Bearer">>,
            <<"expires_in">> => 3600
        },
        Overrides
    ),
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    self() !
        {hex_http_test, oauth_refresh_response, {ok, {200, Headers, term_to_binary(Payload)}}},
    ok.

%% @private
%% Plants the token the next device auth poll hands back.
queue_device_response(AccessToken) ->
    Payload = #{
        <<"access_token">> => AccessToken,
        <<"refresh_token">> => <<"device_refresh">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    self() !
        {hex_http_test, oauth_device_response, {ok, {200, Headers, term_to_binary(Payload)}}},
    ok.

%% @private
put_github_oidc_env(Url) ->
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_URL", Url),
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "request_token").

%% @private
%% The acme organization's repository with no configured credentials, where
%% the build tool keeps Workload Identity outcomes, holds Kept (or what the
%% Kept fun returns on each read), and reports what it is asked to keep to Pid.
workload_identity_config(Pid, Kept) ->
    Read =
        case is_function(Kept, 0) of
            true -> Kept;
            false -> fun() -> Kept end
        end,
    Config = config_with_callbacks(#{
        oauth_tokens => error,
        get_workload_identity_token => fun(<<"hexpm:acme">>) -> Read() end,
        persist_workload_identity_token => fun(RepoName, Result) ->
            Pid ! {workload_identity_persisted, RepoName, Result},
            ok
        end
    }),
    Config#{repo_organization => <<"acme">>, trusted => true}.

%% @private
%% Like workload_identity_config/2, but the outcomes are kept in Store, so
%% concurrent callers see what the others kept.
workload_identity_store_config(Pid, Store) ->
    Config = config_with_callbacks(#{
        oauth_tokens => error,
        get_workload_identity_token => fun(RepoName) ->
            case ets:lookup(Store, RepoName) of
                [{RepoName, Result}] -> Result;
                [] -> error
            end
        end,
        persist_workload_identity_token => fun(RepoName, Result) ->
            ets:insert(Store, {RepoName, Result}),
            Pid ! {workload_identity_persisted, RepoName, Result},
            ok
        end
    }),
    Config#{repo_organization => <<"acme">>, trusted => true}.

%% @private
%% Plants a refusal for the next jwt-bearer token exchange and returns the
%% headers and decoded body it is answered with.
queue_jwt_bearer_refusal() ->
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Body = #{<<"error">> => <<"access_denied">>, <<"error_description">> => <<"No match">>},
    self() ! {hex_http_test, jwt_bearer_response, {ok, {403, Headers, term_to_binary(Body)}}},
    {Headers, Body}.

%% @private
%% Plants the next OIDC audience response the test HTTP adapter will hand back.
queue_oidc_audience_response(Payload) ->
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    self() !
        {hex_http_test, oidc_audience_response, {ok, {200, Headers, term_to_binary(Payload)}}},
    ok.

%% @private
%% Plants the next response the CI provider's own OIDC token endpoint hands
%% back.
queue_ci_token_response(Response) ->
    self() ! {hex_http_test, ci_oidc_token_response, Response},
    ok.

%% @private
%% Plants the next response the jwt-bearer token exchange hands back, merged
%% over a working one so a test only states what it cares about.
queue_jwt_bearer_response(Overrides) ->
    Payload = maps:merge(
        #{
            <<"access_token">> => <<"minted_token">>,
            <<"token_type">> => <<"bearer">>,
            <<"expires_in">> => 900
        },
        Overrides
    ),
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    self() !
        {hex_http_test, jwt_bearer_response, {ok, {200, Headers, term_to_binary(Payload)}}},
    ok.

%% @private
%% The 401 hexpm answers a request whose token it considers expired.
token_expired_response() ->
    Headers = #{<<"www-authenticate">> => <<"Bearer realm=\"hex\", error=\"token_expired\"">>},
    {ok, {401, Headers, <<"">>}}.

%% @private
%% An expired global token whose refresh is about to fail, with the clear
%% callback reporting to Pid so a test can say whether the token was dropped.
refresh_failure_config(Pid) ->
    Now = erlang:system_time(second),
    config_with_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"expired_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now - 100
            }},
        clear_oauth_tokens => fun() ->
            Pid ! cleared,
            ok
        end
    }).

%% @private
%% Whether the device auth lock is held by anyone other than the process asking.
%% A concurrent caller arrives with its own pid as the lock requester id, which
%% is what makes global refuse it while another process holds the lock.
device_auth_lock_held() ->
    Parent = self(),
    spawn(fun() ->
        Id = {{hex_cli_auth, device_auth}, self()},
        case global:set_lock(Id, [node()], 0) of
            true ->
                Parent ! {device_auth_lock, false},
                global:del_lock(Id, [node()]);
            false ->
                Parent ! {device_auth_lock, true}
        end
    end),
    receive
        {device_auth_lock, Held} -> Held
    after 5000 ->
        error(device_auth_lock_probe_timed_out)
    end.

config_with_callbacks(Opts) ->
    ?CONFIG#{cli_auth_callbacks => make_callbacks(Opts)}.

make_callbacks(Opts) ->
    AuthConfig = maps:get(auth_config, Opts, #{}),
    PromptOtp = maps:get(prompt_otp, Opts, fun(_) -> cancelled end),
    ShouldAuthenticate = maps:get(should_authenticate, Opts, fun(_) -> false end),
    PersistFn = maps:get(persist_oauth_tokens, Opts, fun(_, _, _, _) -> ok end),
    ClearFn = maps:get(clear_oauth_tokens, Opts, fun() -> ok end),
    OrganizationReauthFn = maps:get(organization_reauth, Opts, fun(_Organizations) -> ok end),
    DefaultGetOAuthTokens = fun() -> maps:get(oauth_tokens, Opts, error) end,
    GetOAuthTokensFn = maps:get(get_oauth_tokens, Opts, DefaultGetOAuthTokens),

    Callbacks = #{
        get_auth_config => fun(RepoName) -> maps:get(RepoName, AuthConfig, undefined) end,
        get_oauth_tokens => GetOAuthTokensFn,
        persist_oauth_tokens => PersistFn,
        clear_oauth_tokens => ClearFn,
        organization_reauth => OrganizationReauthFn,
        prompt_otp => PromptOtp,
        should_authenticate => ShouldAuthenticate,
        get_client_id => fun() -> <<"test_client">> end
    },
    WorkloadIdentityCallbacks = maps:with(
        [get_workload_identity_token, persist_workload_identity_token], Opts
    ),
    maps:merge(Callbacks, WorkloadIdentityCallbacks).
