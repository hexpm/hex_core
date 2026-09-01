-module(hex_api_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_HTTP_ADAPTER_CONFIG, #{profile => default}).

-define(CONFIG, (hex_core:default_config())#{
    http_adapter => {hex_http_test, ?DEFAULT_HTTP_ADAPTER_CONFIG},
    http_user_agent_fragment => <<"(test)">>,
    api_url => <<"https://api.test">>,
    api_key => <<"dummy">>
}).
% -define(CONFIG, maps:put(api_key, hex_test_helpers:api_key(), hex_core:default_config())).

suite() ->
    [{require, {ssl_certs, [test_pub, test_priv]}}].

all() ->
    [
        package_test,
        release_test,
        replace_test,
        user_test,
        owner_test,
        keys_test,
        auth_test,
        short_url_test,
        oauth_device_flow_test,
        oauth_device_auth_flow_success_test,
        oauth_device_auth_flow_denied_test,
        oauth_device_auth_flow_timeout_test,
        oauth_device_auth_flow_poll_error_test,
        oauth_device_auth_flow_no_refresh_token_test,
        oauth_device_auth_flow_invalid_verification_uri_test,
        oauth_device_auth_flow_malformed_device_response_test,
        oauth_device_auth_flow_malformed_token_response_test,
        oauth_refresh_token_test,
        oauth_sso_authorization_test,
        oauth_device_auth_flow_sso_reauth_test,
        oauth_device_auth_flow_malformed_sso_reauth_test,
        oauth_sso_reauth_required_test,
        oauth_win_cmd_args_escapes_metacharacters_test,
        oauth_revoke_test,
        oauth_client_credentials_test,
        publish_with_expect_header_test,
        publish_without_expect_header_test
    ].

package_test(_Config) ->
    {ok, {200, _, Package}} = hex_api_package:get(?CONFIG, <<"ecto">>),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,

    {ok, {404, _, #{}}} = hex_api_package:get(?CONFIG, <<"nonexisting">>),

    {ok, {200, _, [Package | _]}} = hex_api_package:search(?CONFIG, <<"ecto">>, [
        {sort, downloads}, {page, 1}
    ]),
    #{<<"name">> := <<"ecto">>, <<"releases">> := _} = Package,
    ok.

release_test(_Config) ->
    {ok, {200, _, Release}} = hex_api_release:get(?CONFIG, <<"ecto">>, <<"1.0.0">>),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{
        <<"decimal">> := #{
            <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>
        }
    } = Requirements,
    ok.

publish_test(_Config) ->
    Metadata = #{<<"name">> => <<"ecto">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),
    {ok, {200, _, Release}} = hex_api_release:publish(?CONFIG, Tarball),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{
        <<"decimal">> := #{
            <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>
        }
    } = Requirements,
    ok.

replace_test(_Config) ->
    Metadata = #{<<"name">> => <<"ecto">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),
    {ok, {201, _, Release}} = hex_api_release:publish(?CONFIG, Tarball, [
        {replace, true}
    ]),
    #{<<"version">> := <<"1.0.0">>, <<"requirements">> := Requirements} = Release,
    #{
        <<"decimal">> := #{
            <<"app">> := <<"decimal">>, <<"optional">> := false, <<"requirement">> := <<"~> 1.0">>
        }
    } = Requirements,
    ok.

user_test(_Config) ->
    {ok, {200, _, User}} = hex_api_user:get(?CONFIG, <<"josevalim">>),
    #{<<"username">> := <<"josevalim">>, <<"packages">> := _} = User,
    ok.

owner_test(_Config) ->
    {ok, {200, _, [Owner | _]}} = hex_api_package_owner:list(?CONFIG, <<"decimal">>),
    <<"ericmj">> = maps:get(<<"username">>, Owner),
    ok.

auth_test(_Config) ->
    Params = #{domain => <<"repository">>, resource => <<"gustafson_motors">>},
    {ok, {204, _, nil}} = hex_api_auth:test(?CONFIG, Params),
    ok.

keys_test(_Config) ->
    {ok, {200, _, [Key | _]}} = hex_api_key:list(?CONFIG),
    #{<<"name">> := Name} = Key,

    {ok, {200, _, Key}} = hex_api_key:get(?CONFIG, Name),

    Permissions = [#{<<"domain">> => <<"api">>, <<"resource">> => <<"read">>}],
    {ok, {201, _, Key2}} = hex_api_key:add(?CONFIG, Name, Permissions),
    #{<<"name">> := Name2} = Key2,

    {ok, {200, _, #{<<"name">> := Name2}}} = hex_api_key:delete(?CONFIG, Name2),
    ok.

short_url_test(_Config) ->
    LongURL = <<"https://hex.pm/packages/ecto">>,
    {ok, {201, _, Response}} = hex_api_short_url:create(?CONFIG, LongURL),
    #{<<"url">> := ShortURL} = Response,
    ?assert(is_binary(ShortURL)),
    ?assert(binary:match(ShortURL, <<"https://hex.pm/l/">>) =/= nomatch),
    ok.

oauth_device_flow_test(_Config) ->
    % Test device authorization initiation
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    {ok, {200, _, DeviceResponse}} = hex_api_oauth:device_authorization(?CONFIG, ClientId, Scope),
    #{
        <<"device_code">> := DeviceCode,
        <<"user_code">> := UserCode,
        <<"verification_uri">> := VerificationURI,
        <<"verification_uri_complete">> := VerificationURIComplete,
        <<"expires_in">> := ExpiresIn,
        <<"interval">> := Interval
    } = DeviceResponse,
    ?assert(is_binary(DeviceCode)),
    ?assert(is_binary(UserCode)),
    ?assert(is_binary(VerificationURI)),
    ?assert(is_binary(VerificationURIComplete)),
    ?assert(is_integer(ExpiresIn)),
    ?assert(is_integer(Interval)),

    % Test polling for token (should be pending initially)
    {ok, {400, _, PollResponse}} = hex_api_oauth:poll_device_token(?CONFIG, ClientId, DeviceCode),
    #{<<"error">> := <<"authorization_pending">>} = PollResponse,
    ok.

oauth_device_auth_flow_success_test(_Config) ->
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(VerificationUri, UserCode) ->
        Self ! {prompt_called, VerificationUri, UserCode},
        ok
    end,

    % Queue a success response for when polling happens
    AccessToken = <<"test_access_token">>,
    RefreshToken = <<"test_refresh_token">>,
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

    {ok, Tokens} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser),

    % Verify prompt was called
    receive
        {prompt_called, _Uri, _Code} -> ok
    after 100 ->
        error(prompt_not_called)
    end,

    % Verify tokens
    #{access_token := AccessToken, refresh_token := RefreshToken, expires_at := ExpiresAt} = Tokens,
    ?assert(is_integer(ExpiresAt)),
    ?assert(ExpiresAt > erlang:system_time(second)),
    ok.

oauth_device_auth_flow_denied_test(_Config) ->
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,

    % Queue an access denied response
    ErrorPayload = #{
        <<"error">> => <<"access_denied">>,
        <<"error_description">> => <<"User denied access">>
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response, {ok, {403, Headers, term_to_binary(ErrorPayload)}}},

    {error, {access_denied, 403, _Body}} = hex_api_oauth:device_auth_flow(
        ?CONFIG, ClientId, Scope, PromptUser
    ),
    ok.

oauth_device_auth_flow_timeout_test(_Config) ->
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,

    % Queue an expired token response
    ErrorPayload = #{
        <<"error">> => <<"expired_token">>,
        <<"error_description">> => <<"Device code expired">>
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response, {ok, {400, Headers, term_to_binary(ErrorPayload)}}},

    {error, timeout} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser),
    ok.

oauth_device_auth_flow_poll_error_test(_Config) ->
    % A poll that fails to reach the server keeps polling: the authorization the
    % user is part way through outlives one dropped request.
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,

    SuccessPayload = #{
        <<"access_token">> => <<"test_access_token">>,
        <<"refresh_token">> => <<"test_refresh_token">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self ! {hex_http_test, oauth_device_response, {error, timeout}},
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    {ok, Tokens} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser),

    ?assertEqual(<<"test_access_token">>, maps:get(access_token, Tokens)),
    ok.

oauth_device_auth_flow_no_refresh_token_test(_Config) ->
    % A grant without a refresh token carries no key, rather than a placeholder
    % a build tool would go on to store as if it were a token.
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,

    SuccessPayload = #{
        <<"access_token">> => <<"test_access_token">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    {ok, Tokens} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser),

    ?assertNot(maps:is_key(refresh_token, Tokens)),
    ok.

oauth_device_auth_flow_invalid_verification_uri_test(_Config) ->
    % A verification URI that is not http(s) is not opened, and not a reason to
    % end the flow either.
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},

    DevicePayload = #{
        <<"device_code">> => <<"device_code">>,
        <<"user_code">> => <<"1234-5678">>,
        <<"verification_uri">> => <<"javascript:alert(1)">>,
        <<"verification_uri_complete">> => <<"javascript:alert(1)">>,
        <<"expires_in">> => 600,
        <<"interval">> => 0
    },
    Self !
        {hex_http_test, oauth_device_authorization_response,
            {ok, {200, Headers, term_to_binary(DevicePayload)}}},

    SuccessPayload = #{
        <<"access_token">> => <<"test_access_token">>,
        <<"refresh_token">> => <<"test_refresh_token">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600
    },
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    {ok, Tokens} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser, [
        {open_browser, true}
    ]),

    ?assertEqual(<<"test_access_token">>, maps:get(access_token, Tokens)),
    ok.

oauth_device_auth_flow_malformed_device_response_test(_Config) ->
    % A 200 that does not carry the fields the flow uses is a failed device
    % authorization, not a badmatch or a timer:sleep/1 badarg in the caller.
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> error(prompt_called) end,
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},

    Complete = #{
        <<"device_code">> => <<"device_code">>,
        <<"user_code">> => <<"1234-5678">>,
        <<"verification_uri_complete">> => <<"https://hex.pm/oauth/device?user_code=1234-5678">>,
        <<"expires_in">> => 600,
        <<"interval">> => 0
    },
    Malformed = [
        maps:remove(<<"device_code">>, Complete),
        maps:remove(<<"verification_uri_complete">>, Complete),
        Complete#{<<"interval">> => <<"5">>},
        Complete#{<<"interval">> => -1},
        Complete#{<<"expires_in">> => <<"600">>},
        Complete#{<<"verification_uri_complete">> => 42},
        <<"not a map">>
    ],

    [
        begin
            Self !
                {hex_http_test, oauth_device_authorization_response,
                    {ok, {200, Headers, term_to_binary(Payload)}}},
            ?assertEqual(
                {error, {device_auth_failed, 200, Payload}},
                hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser)
            )
        end
     || Payload <- Malformed
    ],
    ok.

oauth_device_auth_flow_malformed_token_response_test(_Config) ->
    % Same for the poll: a 200 without a usable access token ends the flow with
    % an error the caller already handles.
    ClientId = <<"cli">>,
    Scope = <<"api:write">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},

    Malformed = [
        #{<<"expires_in">> => 3600},
        #{<<"access_token">> => <<"test_access_token">>},
        #{<<"access_token">> => <<"test_access_token">>, <<"expires_in">> => <<"3600">>},
        #{<<"access_token">> => 42, <<"expires_in">> => 3600},
        <<"not a map">>
    ],

    [
        begin
            Self !
                {hex_http_test, oauth_device_response,
                    {ok, {200, Headers, term_to_binary(Payload)}}},
            ?assertEqual(
                {error, {poll_failed, 200, Payload}},
                hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser)
            )
        end
     || Payload <- Malformed
    ],
    ok.

oauth_refresh_token_test(_Config) ->
    % Test token refresh
    ClientId = <<"cli">>,
    RefreshTokenValue = <<"test_refresh_token">>,
    {ok, {200, _, RefreshResponse}} = hex_api_oauth:refresh_token(
        ?CONFIG, ClientId, RefreshTokenValue
    ),
    #{
        <<"access_token">> := NewAccessToken,
        <<"refresh_token">> := NewRefreshToken,
        <<"token_type">> := <<"Bearer">>,
        <<"expires_in">> := ExpiresIn
    } = RefreshResponse,
    ?assert(is_binary(NewAccessToken)),
    ?assert(is_binary(NewRefreshToken)),
    ?assert(is_integer(ExpiresIn)),
    ok.

oauth_sso_authorization_test(_Config) ->
    {ok, {201, _, Response}} = hex_api_oauth:sso_authorization(?CONFIG, [<<"acme">>]),
    #{
        <<"verification_uri">> := VerificationUri,
        <<"expires_in">> := ExpiresIn
    } = Response,
    ?assertEqual(<<"https://hex.pm/sso/authorize/acme">>, VerificationUri),
    ?assert(is_integer(ExpiresIn)),
    ok.

oauth_device_auth_flow_sso_reauth_test(_Config) ->
    % The organizations a token was minted without reach the caller
    ClientId = <<"cli">>,
    Scope = <<"repositories">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,

    SuccessPayload = #{
        <<"access_token">> => <<"test_access_token">>,
        <<"refresh_token">> => <<"test_refresh_token">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600,
        <<"sso_reauth_required">> => [<<"acme">>]
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    {ok, Tokens} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser),

    ?assertEqual([<<"acme">>], maps:get(sso_reauth_required, Tokens)),
    ok.

oauth_device_auth_flow_malformed_sso_reauth_test(_Config) ->
    % A set the server sent in a shape we cannot read carries no key at all. The
    % empty list means "nothing lapsed", which is not what the response said.
    ClientId = <<"cli">>,
    Scope = <<"repositories">>,
    Self = self(),
    PromptUser = fun(_VerificationUri, _UserCode) -> ok end,

    SuccessPayload = #{
        <<"access_token">> => <<"test_access_token">>,
        <<"refresh_token">> => <<"test_refresh_token">>,
        <<"token_type">> => <<"Bearer">>,
        <<"expires_in">> => 3600,
        <<"sso_reauth_required">> => <<"acme">>
    },
    Headers = #{<<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>},
    Self !
        {hex_http_test, oauth_device_response,
            {ok, {200, Headers, term_to_binary(SuccessPayload)}}},

    {ok, Tokens} = hex_api_oauth:device_auth_flow(?CONFIG, ClientId, Scope, PromptUser),

    ?assertNot(maps:is_key(sso_reauth_required, Tokens)),
    ok.

oauth_sso_reauth_required_test(_Config) ->
    % A response that does not carry the field is a server that predates it and
    % means nothing is lapsed; one that carries an unreadable value means the
    % response says nothing at all.
    ?assertEqual({ok, []}, hex_api_oauth:sso_reauth_required(#{})),
    ?assertEqual(
        {ok, []},
        hex_api_oauth:sso_reauth_required(#{<<"sso_reauth_required">> => []})
    ),
    ?assertEqual(
        {ok, [<<"acme">>]},
        hex_api_oauth:sso_reauth_required(#{<<"sso_reauth_required">> => [<<"acme">>]})
    ),
    ?assertEqual(
        error,
        hex_api_oauth:sso_reauth_required(#{<<"sso_reauth_required">> => <<"acme">>})
    ),
    ?assertEqual(
        error,
        hex_api_oauth:sso_reauth_required(#{<<"sso_reauth_required">> => [<<"acme">>, 42]})
    ),
    ?assertEqual(
        error,
        hex_api_oauth:sso_reauth_required(#{<<"sso_reauth_required">> => null})
    ),
    ok.

oauth_win_cmd_args_escapes_metacharacters_test(_Config) ->
    % cmd.exe parses the command line before `start' sees it, and erts only
    % quotes an argument containing whitespace, so a server-supplied URL reaches
    % cmd with its separators inert or it runs whatever trails them.
    ?assertEqual(
        ["/c", "start", "", "https://example.com/^&calc.exe"],
        hex_api_oauth:win_cmd_args("https://example.com/&calc.exe")
    ),
    ?assertEqual(
        ["/c", "start", "", "https://example.com/^%PATH^%"],
        hex_api_oauth:win_cmd_args("https://example.com/%PATH%")
    ),
    ?assertEqual(
        ["/c", "start", "", "^^^&^|^<^>^(^)^\"^%"],
        hex_api_oauth:win_cmd_args("^&|<>()\"%")
    ),
    ?assertEqual(
        ["/c", "start", "", "https://example.com/plain"],
        hex_api_oauth:win_cmd_args("https://example.com/plain")
    ),
    ok.

oauth_revoke_test(_Config) ->
    % Test token revocation
    ClientId = <<"cli">>,
    Token = <<"test_access_token">>,
    {ok, {200, _, nil}} = hex_api_oauth:revoke_token(?CONFIG, ClientId, Token),

    % Test revoking non-existent token (should still return 200)
    NonExistentToken = <<"non_existent_token">>,
    {ok, {200, _, nil}} = hex_api_oauth:revoke_token(?CONFIG, ClientId, NonExistentToken),
    ok.

oauth_client_credentials_test(_Config) ->
    % Test client credentials token exchange without options
    ClientId = <<"cli">>,
    ApiKey = <<"test_api_key">>,
    Scope = <<"api">>,
    {ok, {200, _, TokenResponse}} = hex_api_oauth:client_credentials_token(
        ?CONFIG, ClientId, ApiKey, Scope
    ),
    #{
        <<"access_token">> := AccessToken,
        <<"token_type">> := <<"bearer">>,
        <<"expires_in">> := ExpiresIn,
        <<"scope">> := Scope
    } = TokenResponse,
    ?assert(is_binary(AccessToken)),
    ?assert(is_integer(ExpiresIn)),
    % Client credentials grant should not return a refresh token
    ?assertEqual(false, maps:is_key(<<"refresh_token">>, TokenResponse)),

    % Test client credentials token exchange with name option
    Name = <<"MyMachine">>,
    {ok, {200, _, TokenResponse2}} = hex_api_oauth:client_credentials_token(
        ?CONFIG, ClientId, ApiKey, Scope, [{name, Name}]
    ),
    #{
        <<"access_token">> := AccessToken2,
        <<"token_type">> := <<"bearer">>,
        <<"expires_in">> := ExpiresIn2
    } = TokenResponse2,
    ?assert(is_binary(AccessToken2)),
    ?assert(is_integer(ExpiresIn2)),
    ok.

publish_with_expect_header_test(_Config) ->
    % Test that send_100_continue => true includes Expect: 100-continue header
    Metadata = #{<<"name">> => <<"expect_test">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),

    % Default config has send_100_continue => true
    Config = ?CONFIG,
    {ok, {200, _, Release}} = hex_api_release:publish(Config, Tarball),
    #{<<"version">> := <<"1.0.0">>} = Release,
    ok.

publish_without_expect_header_test(_Config) ->
    % Test that send_100_continue => false does not include Expect header
    Metadata = #{<<"name">> => <<"no_expect_test">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),

    % Explicitly disable send_100_continue
    Config = maps:put(send_100_continue, false, ?CONFIG),
    {ok, {200, _, Release}} = hex_api_release:publish(Config, Tarball),
    #{<<"version">> := <<"1.0.0">>} = Release,
    ok.
