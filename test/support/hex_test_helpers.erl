-module(hex_test_helpers).
-export([api_key/0]).

api_key() ->
    case os:getenv("TEST_API_KEY") of
        false -> nil;
        Token -> list_to_binary(Token)
    end.
