-module(hex_http_test).
-behaviour(hex_http).
-export([request/3]).
-define(TEST_REPO_URI, "https://repo.test").
-define(TEST_API_URI, "https://api.test").
-define(PRIVATE_KEY, hex_test_helpers:fixture("test_priv.pem")).
-define(PUBLIC_KEY, hex_test_helpers:fixture("test_pub.pem")).

%%====================================================================
%% API functions
%%====================================================================

request(_Method, URI, Headers) when is_binary(URI) and is_map(Headers) ->
    fixture(URI, Headers).

%%====================================================================
%% Internal functions
%%====================================================================

api_headers() ->
    #{
        <<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>
    }.

fixture(_, #{<<"if-none-match">> := <<"\"dummy\"">> = ETag}) ->
    Headers = #{
      <<"etag">> => ETag
    },
    {ok, {304, Headers, <<"">>}};

%% Repo API

fixture(<<?TEST_REPO_URI, "/names">>, _) ->
    Names = #{
        packages => [
            #{name => <<"ecto">>}
        ]
    },
    Payload = hex_registry:encode_names(Names),
    Signed = hex_registry:sign_protobuf(Payload, ?PRIVATE_KEY),
    Compressed = zlib:gzip(Signed),
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    {ok, {200, Headers, Compressed}};

fixture(<<?TEST_REPO_URI, "/versions">>, _) ->
    Versions = #{
        packages => [
            #{name => <<"ecto">>, versions => [<<"1.0.0">>]}
        ]
    },
    Payload = hex_registry:encode_versions(Versions),
    Signed = hex_registry:sign_protobuf(Payload, ?PRIVATE_KEY),
    Compressed = zlib:gzip(Signed),
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    {ok, {200, Headers, Compressed}};

fixture(<<?TEST_REPO_URI, "/packages/ecto">>, _) ->
    Package = #{
        releases => [
            #{
                version => <<"1.0.0">>,
                checksum => <<"dummy">>,
                dependencies => []
            }
        ]
    },
    Payload = hex_registry:encode_package(Package),
    Signed = hex_registry:sign_protobuf(Payload, ?PRIVATE_KEY),
    Compressed = zlib:gzip(Signed),
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    {ok, {200, Headers, Compressed}};

fixture(<<?TEST_REPO_URI, "/tarballs/ecto-1.0.0.tar">>, _) ->
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    {ok, {Tarball, _Checksum}} = hex_tarball:create(#{<<"name">> => <<"ecto">>}, []),
    {ok, {200, Headers, Tarball}};

fixture(<<?TEST_REPO_URI, _/binary>>, _) ->
    {ok, {403, #{}, <<"not found">>}};

%% HTTP API

fixture(<<?TEST_API_URI, "/users/josevalim">>, _) ->
    Payload = #{
        <<"username">> => <<"josevalim">>,
        <<"packages">> => []
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /packages/:package

fixture(<<?TEST_API_URI, "/packages/ecto">>, _) ->
    Payload = #{
        <<"name">> => <<"ecto">>,
        <<"releases">> => []
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /packages/:package/releases/:version

fixture(<<?TEST_API_URI, "/packages/ecto/releases/1.0.0">>, _) ->
    Payload = #{
        <<"version">> => <<"1.0.0">>,
        <<"requirements">> => #{
            <<"decimal">> => #{
                <<"requirement">> => <<"~> 1.0">>,
                <<"optional">> => false,
                <<"app">> => <<"decimal">>
            }
        }
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /packages

fixture(<<?TEST_API_URI, "/packages?search=ecto", _/binary>>, _) ->
    Payload = [
        #{
            <<"name">> => <<"ecto">>,
            <<"releases">> => []
        }
    ],
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /packages/:package/owners

fixture(<<?TEST_API_URI, "/packages/decimal/owners", _/binary>>, #{<<"authorization">> := Token}) when is_binary(Token) ->
    Payload = [
        #{
            <<"username">> => <<"ericmj">>
        }
    ],
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(<<?TEST_API_URI, "/packages/decimal/owners", _/binary>>, _) ->
    {ok, {401, api_headers(), <<"">>}};

%% /keys

fixture(<<?TEST_API_URI, "/keys">>, #{<<"authorization">> := Token}) when is_binary(Token) ->
    Payload = [
        #{
            <<"name">> => <<"key-1">>
        }
    ],
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /keys/:name

fixture(<<?TEST_API_URI, "/keys/", Name/binary>>, #{<<"authorization">> := Token}) when is_binary(Token) ->
    Payload = #{
        <<"name">> => Name
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(<<?TEST_API_URI, "/keys", _/binary>>, _) ->
    {ok, {401, api_headers(), <<"">>}};

%% Other

fixture(URI, _) ->
    error({no_fixture, URI}).
