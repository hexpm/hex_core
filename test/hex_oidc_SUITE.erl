-module(hex_oidc_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(CONFIG, (hex_core:default_config())#{
    http_adapter => {hex_http_test, #{profile => default}},
    http_user_agent_fragment => <<"(test)">>
}).

-define(GITHUB_ENV_VARS, [
    "ACTIONS_ID_TOKEN_REQUEST_URL", "ACTIONS_ID_TOKEN_REQUEST_TOKEN"
]).

all() ->
    [
        detect_provider_absent_test,
        detect_provider_empty_url_test,
        detect_provider_empty_token_test,
        detect_provider_github_actions_test,
        fetch_token_success_test,
        fetch_token_non_200_test,
        fetch_token_transport_error_test,
        fetch_token_missing_value_test,
        fetch_token_empty_value_test,
        put_audience_no_query_test,
        put_audience_existing_query_test,
        extract_jwt_value_success_test,
        extract_jwt_value_whitespace_and_colon_test,
        extract_jwt_value_missing_key_test,
        extract_jwt_value_empty_token_test,
        extract_jwt_value_unterminated_test
    ].

init_per_testcase(_TestCase, Config) ->
    lists:foreach(fun os:unsetenv/1, ?GITHUB_ENV_VARS),
    Config.

end_per_testcase(_TestCase, _Config) ->
    lists:foreach(fun os:unsetenv/1, ?GITHUB_ENV_VARS),
    ok.

%%====================================================================
%% Test Cases - detect_provider
%%====================================================================

detect_provider_absent_test(_Config) ->
    ?assertEqual(none, hex_oidc:detect_provider()),
    ok.

detect_provider_empty_url_test(_Config) ->
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_URL", ""),
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "a_token"),
    ?assertEqual(none, hex_oidc:detect_provider()),
    ok.

detect_provider_empty_token_test(_Config) ->
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_URL", "https://ci.test/token"),
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", ""),
    ?assertEqual(none, hex_oidc:detect_provider()),
    ok.

detect_provider_github_actions_test(_Config) ->
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_URL", "https://ci.test/token"),
    os:putenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "request_token"),
    ?assertEqual(
        {ok,
            {github_actions, #{
                url => <<"https://ci.test/token">>, request_token => <<"request_token">>
            }}},
        hex_oidc:detect_provider()
    ),
    ok.

%%====================================================================
%% Test Cases - fetch_token
%%====================================================================

fetch_token_success_test(_Config) ->
    Provider =
        {github_actions, #{
            url => <<"https://ci.test/token">>, request_token => <<"request_token">>
        }},
    queue_ci_token_response({ok, {200, #{}, <<"{\"count\":1,\"value\":\"the.jwt.token\"}">>}}),
    ?assertEqual({ok, <<"the.jwt.token">>}, hex_oidc:fetch_token(?CONFIG, Provider, <<"hexpm">>)),
    ok.

fetch_token_non_200_test(_Config) ->
    Provider =
        {github_actions, #{
            url => <<"https://ci.test/token">>, request_token => <<"request_token">>
        }},
    queue_ci_token_response({ok, {403, #{}, <<"">>}}),
    ?assertEqual(
        {error, {oidc_token_request_failed, 403}},
        hex_oidc:fetch_token(?CONFIG, Provider, <<"hexpm">>)
    ),
    ok.

fetch_token_transport_error_test(_Config) ->
    Provider =
        {github_actions, #{
            url => <<"https://ci.test/token">>, request_token => <<"request_token">>
        }},
    queue_ci_token_response({error, timeout}),
    ?assertEqual(
        {error, {oidc_token_unavailable, timeout}},
        hex_oidc:fetch_token(?CONFIG, Provider, <<"hexpm">>)
    ),
    ok.

fetch_token_missing_value_test(_Config) ->
    Provider =
        {github_actions, #{
            url => <<"https://ci.test/token">>, request_token => <<"request_token">>
        }},
    queue_ci_token_response({ok, {200, #{}, <<"{\"count\":1}">>}}),
    ?assertEqual(
        {error, oidc_token_missing}, hex_oidc:fetch_token(?CONFIG, Provider, <<"hexpm">>)
    ),
    ok.

fetch_token_empty_value_test(_Config) ->
    Provider =
        {github_actions, #{
            url => <<"https://ci.test/token">>, request_token => <<"request_token">>
        }},
    queue_ci_token_response({ok, {200, #{}, <<"{\"count\":1,\"value\":\"\"}">>}}),
    ?assertEqual(
        {error, oidc_token_missing}, hex_oidc:fetch_token(?CONFIG, Provider, <<"hexpm">>)
    ),
    ok.

%%====================================================================
%% Test Cases - put_audience
%%====================================================================

put_audience_no_query_test(_Config) ->
    ?assertEqual(
        <<"https://ci.test/token?audience=hexpm">>,
        hex_oidc:put_audience(<<"https://ci.test/token">>, <<"hexpm">>)
    ),
    ok.

put_audience_existing_query_test(_Config) ->
    ?assertEqual(
        <<"https://ci.test/token?api-version=2.0&audience=hexpm">>,
        hex_oidc:put_audience(<<"https://ci.test/token?api-version=2.0">>, <<"hexpm">>)
    ),
    ok.

%%====================================================================
%% Test Cases - extract_jwt_value (no-JSON-decoder fallback)
%%====================================================================

extract_jwt_value_success_test(_Config) ->
    ?assertEqual(
        {ok, <<"abc-DEF_123.ghi">>},
        hex_oidc:extract_jwt_value(<<"{\"count\":1,\"value\":\"abc-DEF_123.ghi\"}">>)
    ),
    ok.

extract_jwt_value_whitespace_and_colon_test(_Config) ->
    ?assertEqual(
        {ok, <<"the.jwt.token">>},
        hex_oidc:extract_jwt_value(<<"{\"value\"   :   \"the.jwt.token\"}">>)
    ),
    ok.

extract_jwt_value_missing_key_test(_Config) ->
    ?assertEqual(error, hex_oidc:extract_jwt_value(<<"{\"count\":1}">>)),
    ok.

extract_jwt_value_empty_token_test(_Config) ->
    ?assertEqual(error, hex_oidc:extract_jwt_value(<<"{\"value\":\"\"}">>)),
    ok.

extract_jwt_value_unterminated_test(_Config) ->
    ?assertEqual(error, hex_oidc:extract_jwt_value(<<"{\"value\":\"abc.def">>)),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
queue_ci_token_response(Response) ->
    self() ! {hex_http_test, ci_oidc_token_response, Response},
    ok.
