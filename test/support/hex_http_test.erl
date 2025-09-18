-module(hex_http_test).
-behaviour(hex_http).
-export([request/5]).
-define(TEST_REPO_URL, "https://repo.test").
-define(TEST_API_URL, "https://api.test").
-define(PRIVATE_KEY, ct:get_config({ssl_certs, test_priv})).
-define(PUBLIC_KEY, ct:get_config({ssl_certs, test_pub})).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, Headers, Body, _Options) when is_binary(URI) and is_map(Headers) ->
    fixture(Method, URI, Headers, Body).

%%====================================================================
%% Internal functions
%%====================================================================

api_headers() ->
    #{
        <<"content-type">> => <<"application/vnd.hex+erlang; charset=utf-8">>
    }.

fixture(get, _, #{<<"if-none-match">> := <<"\"dummy\"">> = ETag}, _) ->
    Headers = #{
      <<"etag">> => ETag
    },
    {ok, {304, Headers, <<"">>}};

%% Repo API

fixture(get, <<?TEST_REPO_URL, "/names">>, _, _) ->
    Names = #{
        repository => <<"hexpm">>,
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

fixture(get, <<?TEST_REPO_URL, "/versions">>, _, _) ->
    Versions = #{
        repository => <<"hexpm">>,
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

fixture(get, <<?TEST_REPO_URL, "/packages/ecto">>, _, _) ->
    Package = #{
        repository => <<"hexpm">>,
        name => <<"ecto">>,
        releases => [
            #{
                version => <<"1.0.0">>,
                inner_checksum => <<"dummy">>,
                outer_checksum => <<"dummy">>,
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

fixture(get, <<?TEST_REPO_URL, "/tarballs/ecto-1.0.0.tar">>, _, _) ->
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    Metadata = #{
        <<"name">> => <<"ecto">>,
        <<"version">> => <<"1.0.0">>
    },
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),
    {ok, {200, Headers, Tarball}};

fixture(get, <<?TEST_REPO_URL, "/docs/ecto-1.0.0.tar.gz">>, _, _) ->
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    {ok, Docs} = hex_tarball:create_docs([]),
    {ok, {200, Headers, Docs}};

fixture(get, <<?TEST_REPO_URL, "/installs/hex-1.x.csv">>, _, _) ->
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    CSV = <<"1.0.0,abc123,1.13.0\n1.1.0,def456,1.14.0\n">>,
    {ok, {200, Headers, CSV}};

fixture(get, <<?TEST_REPO_URL, "/public_key">>, _, _) ->
    Headers = #{
      <<"etag">> => <<"\"dummy\"">>
    },
    {ok, {200, Headers, ?PUBLIC_KEY}};

fixture(get, <<?TEST_REPO_URL, _/binary>>, _, _) ->
    {ok, {403, #{}, <<"not found">>}};

%% HTTP API

fixture(get, <<?TEST_API_URL, "/auth?domain=repository&resource=gustafson_motors">>, _, _) ->
    {ok, {204, api_headers(), term_to_binary(nil)}};

fixture(get, <<?TEST_API_URL, "/users/josevalim">>, _, _) ->
    Payload = #{
        <<"username">> => <<"josevalim">>,
        <<"packages">> => []
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /packages/:package

fixture(get, <<?TEST_API_URL, "/packages/ecto">>, _, _) ->
    Payload = #{
        <<"name">> => <<"ecto">>,
        <<"releases">> => []
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(get, <<?TEST_API_URL, "/packages/nonexisting">>, _, _) ->
    Payload = #{
        <<"message">> => <<"Page not found">>,
        <<"status">> => 404
    },
    {ok, {404, api_headers(), term_to_binary(Payload)}};

%% /packages/:package/releases/:version

fixture(get, <<?TEST_API_URL, "/packages/ecto/releases/1.0.0">>, _, _) ->
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

%% /publish

fixture(get, <<?TEST_API_URL, "/publish">>, _, _) ->
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

%% /publish?replace=true

fixture(post, <<?TEST_API_URL, "/publish?replace=true">>, _, _) ->
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
    {ok, {201, api_headers(), term_to_binary(Payload)}};

%% /packages

fixture(get, <<?TEST_API_URL, "/packages?search=ecto", _/binary>>, _, _) ->
    Payload = [
        #{
            <<"name">> => <<"ecto">>,
            <<"releases">> => []
        }
    ],
    {ok, {200, api_headers(), term_to_binary(Payload)}};

%% /packages/:package/owners

fixture(get, <<?TEST_API_URL, "/packages/decimal/owners", _/binary>>, #{<<"authorization">> := Token}, _) when is_binary(Token) ->
    Payload = [
        #{
            <<"username">> => <<"ericmj">>
        }
    ],
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(get, <<?TEST_API_URL, "/packages/decimal/owners", _/binary>>, _, _) ->
    {ok, {401, api_headers(), <<"">>}};

%% /keys

fixture(get, <<?TEST_API_URL, "/keys">>, #{<<"authorization">> := Token}, _) when is_binary(Token) ->
    Payload = [
        #{
            <<"name">> => <<"key-1">>
        }
    ],
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(get, <<?TEST_API_URL, "/keys/", Name/binary>>, #{<<"authorization">> := Token}, _) when is_binary(Token) ->
    Payload = #{
        <<"name">> => Name
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(get, <<?TEST_API_URL, "/keys", _/binary>>, _, _) ->
    {ok, {401, api_headers(), <<"">>}};

fixture(post, <<?TEST_API_URL, "/keys">>, #{<<"authorization">> := Token}, {_, Body}) when is_binary(Token) ->
    {ok, {201, api_headers(), Body}};

fixture(delete, <<?TEST_API_URL, "/keys/", Name/binary>>, #{<<"authorization">> := Token}, _) when is_binary(Token) ->
    Payload = #{
        <<"name">> => Name
    },
    {ok, {200, api_headers(), term_to_binary(Payload)}};

fixture(post, <<?TEST_API_URL, "/short_url">>, _, {_, Body}) ->
    DecodedBody = binary_to_term(Body),
    #{<<"url">> := URL} = DecodedBody,
    ShortURL = <<"https://hex.pm/l/", (integer_to_binary(erlang:phash2(URL)))/binary>>,
    Payload = #{
        <<"url">> => ShortURL
    },
    {ok, {201, api_headers(), term_to_binary(Payload)}};

%% Other

fixture(Method, URI, _, _) ->
    error({no_fixture, Method, URI}).
