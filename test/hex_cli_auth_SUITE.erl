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

        %% with_api tests - wrapper behavior
        with_api_optional_test,
        with_api_auth_inline_test,
        with_api_device_auth_test,

        %% with_repo tests - wrapper behavior
        with_repo_optional_test,
        with_repo_trusted_with_auth_test
    ].

%%====================================================================
%% Test Cases - resolve_api_auth
%%====================================================================

resolve_api_auth_config_passthrough_test(_Config) ->
    %% When api_key is already in config, it should be used directly
    Callbacks = make_callbacks(#{}),
    ConfigWithKey = ?CONFIG#{api_key => <<"config_api_key">>},

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(Callbacks, read, ConfigWithKey),
    ?assertEqual(<<"config_api_key">>, ApiKey),
    ?assertEqual(#{source => config, has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_per_repo_test(_Config) ->
    %% Test per-repo api_key from callback
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{api_key => <<"repo_api_key">>}}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(Callbacks, write, ?CONFIG),
    ?assertEqual(<<"repo_api_key">>, ApiKey),
    ?assertEqual(#{source => config, has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_parent_repo_test(_Config) ->
    %% Test parent repo fallback for "hexpm:org" repos
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{api_key => <<"parent_api_key">>}}
    }),

    {ok, ApiKey, _} = hex_cli_auth:resolve_api_auth(
        Callbacks, write, ?CONFIG#{repo_name => <<"hexpm:myorg">>}
    ),
    ?assertEqual(<<"parent_api_key">>, ApiKey),
    ok.

resolve_api_auth_oauth_test(_Config) ->
    %% Test OAuth token fallback with valid token
    Now = erlang:system_time(second),
    Callbacks = make_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"oauth_token">>,
                refresh_token => <<"refresh_token">>,
                expires_at => Now + 3600
            }}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(Callbacks, read, ?CONFIG),
    ?assertEqual(<<"Bearer oauth_token">>, ApiKey),
    ?assertEqual(#{source => oauth, has_refresh_token => true}, AuthContext),
    ok.

resolve_api_auth_oauth_expired_refresh_test(_Config) ->
    %% Test OAuth token refresh when expired
    Now = erlang:system_time(second),
    Self = self(),
    Callbacks = make_callbacks(#{
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

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(Callbacks, read, ?CONFIG),
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
    Callbacks = make_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"oauth_token">>,
                expires_at => Now + 3600
            }}
    }),

    {ok, ApiKey, AuthContext} = hex_cli_auth:resolve_api_auth(Callbacks, read, ?CONFIG),
    ?assertEqual(<<"Bearer oauth_token">>, ApiKey),
    ?assertEqual(#{source => oauth, has_refresh_token => false}, AuthContext),
    ok.

resolve_api_auth_no_auth_test(_Config) ->
    %% Test when no auth is available
    Callbacks = make_callbacks(#{
        auth_config => #{},
        oauth_tokens => error
    }),

    Result = hex_cli_auth:resolve_api_auth(Callbacks, read, ?CONFIG),
    ?assertEqual({error, no_auth}, Result),
    ok.

%%====================================================================
%% Test Cases - resolve_repo_auth
%%====================================================================

resolve_repo_auth_config_passthrough_test(_Config) ->
    %% When repo_key is already in config, it should be used directly
    Callbacks = make_callbacks(#{}),
    ConfigWithKey = ?CONFIG#{repo_key => <<"config_repo_key">>},

    {ok, RepoKey, AuthContext} = hex_cli_auth:resolve_repo_auth(Callbacks, ConfigWithKey),
    ?assertEqual(<<"config_repo_key">>, RepoKey),
    ?assertEqual(#{source => config, has_refresh_token => false}, AuthContext),
    ok.

resolve_repo_auth_callback_repo_key_test(_Config) ->
    %% Test repo_key from get_auth_config callback
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{repo_key => <<"callback_repo_key">>}}
    }),

    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Callbacks, ?CONFIG#{trusted => true}),
    ?assertEqual(<<"callback_repo_key">>, RepoKey),
    ok.

resolve_repo_auth_trusted_auth_key_test(_Config) ->
    %% Test trusted + auth_key (no oauth_exchange) uses auth_key directly
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"auth_key_value">>}}
    }),

    Config = ?CONFIG#{trusted => true, oauth_exchange => false},
    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
    ?assertEqual(<<"auth_key_value">>, RepoKey),
    ok.

resolve_repo_auth_untrusted_ignores_auth_key_test(_Config) ->
    %% Test untrusted config ignores auth_key even when present
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"auth_key_value">>}},
        oauth_tokens => error
    }),

    Config = ?CONFIG#{trusted => false},
    Result = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
    ?assertEqual(no_auth, Result),
    ok.

resolve_repo_auth_oauth_fallback_test(_Config) ->
    %% Test fallback to global OAuth when trusted but no auth_key
    Now = erlang:system_time(second),
    Callbacks = make_callbacks(#{
        auth_config => #{},
        oauth_tokens =>
            {ok, #{
                access_token => <<"global_oauth">>,
                expires_at => Now + 3600
            }}
    }),

    Config = ?CONFIG#{trusted => true},
    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
    ?assertEqual(<<"Bearer global_oauth">>, RepoKey),
    ok.

resolve_repo_auth_no_auth_test(_Config) ->
    %% Test no_auth when untrusted and no credentials
    Callbacks = make_callbacks(#{
        auth_config => #{},
        oauth_tokens => error
    }),

    Config = ?CONFIG#{trusted => false},
    Result = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
    ?assertEqual(no_auth, Result),
    ok.

resolve_repo_auth_oauth_exchange_new_token_test(_Config) ->
    %% Test oauth_exchange with auth_key but no existing oauth_token
    Self = self(),
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"my_auth_key">>}},
        persist_oauth_tokens => fun(Scope, Access, _Refresh, Expires) ->
            Self ! {persisted, Scope, Access, Expires},
            ok
        end
    }),

    Config = ?CONFIG#{trusted => true, oauth_exchange => true},
    {ok, RepoKey, AuthContext} = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
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
    Callbacks = make_callbacks(#{
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

    Config = ?CONFIG#{trusted => true, oauth_exchange => true},
    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
    ?assertEqual(<<"Bearer existing_token">>, RepoKey),
    ok.

resolve_repo_auth_oauth_exchange_existing_expired_test(_Config) ->
    %% Test oauth_exchange with expired oauth_token re-exchanges
    Now = erlang:system_time(second),
    Self = self(),
    Callbacks = make_callbacks(#{
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

    Config = ?CONFIG#{trusted => true, oauth_exchange => true},
    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
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
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"parent_auth_key">>}}
    }),

    Config = ?CONFIG#{repo_name => <<"hexpm:myorg">>, trusted => true, oauth_exchange => false},
    {ok, RepoKey, _} = hex_cli_auth:resolve_repo_auth(Callbacks, Config),
    ?assertEqual(<<"parent_auth_key">>, RepoKey),
    ok.

%%====================================================================
%% Test Cases - with_api OTP handling
%%====================================================================

with_api_otp_required_test(_Config) ->
    %% Test OTP prompt when server returns otp_required
    Now = erlang:system_time(second),
    Callbacks = make_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) -> {ok, <<"123456">>} end
    }),

    CallCount = counters:new(1, []),
    Result = hex_cli_auth:with_api(
        Callbacks,
        write,
        ?CONFIG,
        fun(Config) ->
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
                    ?assertEqual(<<"123456">>, maps:get(api_otp, Config)),
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
    Callbacks = make_callbacks(#{
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
        Callbacks,
        write,
        ?CONFIG,
        fun(Config) ->
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
                    ?assertEqual(<<"wrong_otp">>, maps:get(api_otp, Config)),
                    {ok,
                        {401,
                            #{
                                <<"www-authenticate">> =>
                                    <<"Bearer realm=\"hex\", error=\"invalid_totp\"">>
                            },
                            <<>>}};
                _ ->
                    ?assertEqual(<<"correct_otp">>, maps:get(api_otp, Config)),
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
    Callbacks = make_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) -> cancelled end
    }),

    Result = hex_cli_auth:with_api(
        Callbacks,
        write,
        ?CONFIG,
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
    Callbacks = make_callbacks(#{
        oauth_tokens =>
            {ok, #{
                access_token => <<"token">>,
                expires_at => Now + 3600
            }},
        prompt_otp => fun(_Msg) -> {ok, <<"wrong_otp">>} end
    }),

    Result = hex_cli_auth:with_api(
        Callbacks,
        write,
        ?CONFIG,
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
    Callbacks = make_callbacks(#{
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
        Callbacks,
        write,
        ?CONFIG,
        fun(Config) ->
            Count = counters:get(CallCount, 1),
            counters:add(CallCount, 1, 1),
            ApiKey = maps:get(api_key, Config),
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
%% Test Cases - with_api (wrapper behavior)
%%====================================================================

with_api_optional_test(_Config) ->
    %% Test optional => true allows requests without auth
    Callbacks = make_callbacks(#{oauth_tokens => error}),

    %% Function is called without api_key
    Result = hex_cli_auth:with_api(
        Callbacks,
        read,
        ?CONFIG,
        fun(Config) -> maps:get(api_key, Config, undefined) end,
        [{optional, true}]
    ),
    ?assertEqual(undefined, Result),
    ok.

with_api_auth_inline_test(_Config) ->
    %% Test auth_inline => false returns error instead of prompting
    Callbacks = make_callbacks(#{oauth_tokens => error}),

    Result = hex_cli_auth:with_api(
        Callbacks,
        read,
        ?CONFIG,
        fun(_) -> error(should_not_be_called) end,
        [{optional, false}, {auth_inline, false}]
    ),
    ?assertEqual({error, {auth_error, no_credentials}}, Result),
    ok.

with_api_device_auth_test(_Config) ->
    %% Test device auth flow when should_authenticate returns true
    Self = self(),
    Callbacks = make_callbacks(#{
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
        Callbacks,
        write,
        ?CONFIG,
        fun(Config) -> maps:get(api_key, Config) end,
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
    Callbacks = make_callbacks(#{oauth_tokens => error}),

    Result = hex_cli_auth:with_repo(
        Callbacks,
        ?CONFIG#{trusted => false},
        fun(Config) -> maps:get(repo_key, Config, undefined) end
    ),
    ?assertEqual(undefined, Result),
    ok.

with_repo_trusted_with_auth_test(_Config) ->
    %% Test with_repo with trusted config and auth_key
    Callbacks = make_callbacks(#{
        auth_config => #{<<"hexpm">> => #{auth_key => <<"my_auth_key">>}}
    }),

    Result = hex_cli_auth:with_repo(
        Callbacks,
        ?CONFIG#{trusted => true, oauth_exchange => false},
        fun(Config) -> maps:get(repo_key, Config) end
    ),
    ?assertEqual(<<"my_auth_key">>, Result),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

make_callbacks(Opts) ->
    AuthConfig = maps:get(auth_config, Opts, #{}),
    PromptOtp = maps:get(prompt_otp, Opts, fun(_) -> cancelled end),
    ShouldAuthenticate = maps:get(should_authenticate, Opts, fun(_) -> false end),
    PersistFn = maps:get(persist_oauth_tokens, Opts, fun(_, _, _, _) -> ok end),
    DefaultGetOAuthTokens = fun() -> maps:get(oauth_tokens, Opts, error) end,
    GetOAuthTokensFn = maps:get(get_oauth_tokens, Opts, DefaultGetOAuthTokens),

    #{
        get_auth_config => fun(RepoName) -> maps:get(RepoName, AuthConfig, undefined) end,
        get_oauth_tokens => GetOAuthTokensFn,
        persist_oauth_tokens => PersistFn,
        prompt_otp => PromptOtp,
        should_authenticate => ShouldAuthenticate,
        get_client_id => fun() -> <<"test_client">> end
    }.
