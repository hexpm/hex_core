-module(hex_http_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        bearer_error_test,
        bearer_error_parameters_test,
        bearer_error_several_challenges_test,
        bearer_error_quoted_string_test,
        bearer_error_none_test
    ].

bearer_error_test(_Config) ->
    %% What hexpm and repo.hex.pm send
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(<<"Bearer realm=\"hex\", error=\"invalid_token\"">>)
    ),
    ?assertEqual(
        <<"insufficient_scope">>,
        bearer_error(<<"Bearer realm=\"hex\", error=\"insufficient_scope\"">>)
    ),
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(
            <<"Bearer realm=\"hex\", error=\"invalid_token\", ",
                "error_description=\"The access token expired\"">>
        )
    ),
    ok.

bearer_error_parameters_test(_Config) ->
    %% Scheme and parameter names are case-insensitive, parameters come in
    %% any order, a value can be a token, there can be whitespace around "="
    %% and realm is optional (RFC 9110 section 11.2).
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(<<"bearer ERROR=\"invalid_token\", Realm=\"hex\"">>)
    ),
    ?assertEqual(<<"invalid_token">>, bearer_error(<<"Bearer error = invalid_token">>)),
    ?assertEqual(<<"invalid_token">>, bearer_error(<<"Bearer error=\"invalid_token\"">>)),
    ?assertEqual(
        <<"invalid_token">>, bearer_error(<<"Bearer realm=\"hex\",\terror=\"invalid_token\"">>)
    ),
    ok.

bearer_error_several_challenges_test(_Config) ->
    %% A field can hold several challenges, and a challenge can be a
    %% token68 (RFC 9110 section 11.6.1).
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(<<"Basic realm=\"hex\", Bearer realm=\"hex\", error=\"invalid_token\"">>)
    ),
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(<<"Negotiate a87421000492aa874209af8bc028==, Bearer error=\"invalid_token\"">>)
    ),
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(<<", , Bearer error=\"invalid_token\",, realm=\"hex\"">>)
    ),
    ?assertEqual(
        undefined,
        bearer_error(
            <<"Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\"\", ",
                "Basic realm=\"simple\"">>
        )
    ),
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(
            <<"Newauth realm=\"apps\", type=1, title=\"Login to \\\"apps\\\"\", ",
                "Bearer realm=\"hex\", error=\"invalid_token\"">>
        )
    ),
    ok.

bearer_error_quoted_string_test(_Config) ->
    %% A quoted string can hold commas, escaped quotes and things that look
    %% like parameters.
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(
            <<"Bearer error_description=\"say \\\"hi\\\", error=\\\"nope\\\"\", ",
                "error=\"invalid_token\"">>
        )
    ),
    ?assertEqual(<<"a\"b">>, bearer_error(<<"Bearer error=\"a\\\"b\"">>)),
    ok.

bearer_error_none_test(_Config) ->
    ?assertEqual(undefined, hex_http:bearer_error(#{})),
    ?assertEqual(undefined, bearer_error(<<"Bearer realm=\"hex\"">>)),
    ?assertEqual(undefined, bearer_error(<<"Basic realm=\"hex\"">>)),
    ?assertEqual(undefined, bearer_error(<<"Bearer">>)),
    ?assertEqual(undefined, bearer_error(<<"">>)),
    ?assertEqual(undefined, bearer_error(<<"Bearer error=\"unterminated">>)),
    %% A parameter read before the field turned malformed is kept
    ?assertEqual(
        <<"invalid_token">>,
        bearer_error(<<"Bearer error=\"invalid_token\", error_description=\"unterminated">>)
    ),
    ok.

bearer_error(Value) ->
    hex_http:bearer_error(#{<<"www-authenticate">> => Value}).
