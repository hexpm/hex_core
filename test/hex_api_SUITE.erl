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
        oauth_refresh_token_test,
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
