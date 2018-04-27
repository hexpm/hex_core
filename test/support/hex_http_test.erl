-module(hex_http_test).
-behaviour(hex_http).
-export([get/2]).
-define(TEST_REPO_URI, "https://repo.test").
-define(TEST_API_URI, "https://api.test").
-define(PRIVATE_KEY, element(2, file:read_file("test/fixtures/test_priv.pem"))).
-define(PUBLIC_KEY, element(2, file:read_file("test/fixtures/test_pub.pem"))).

%%====================================================================
%% API functions
%%====================================================================

get(URI, Headers) when is_binary(URI) and is_map(Headers) ->
    fixture(URI, Headers).

%%====================================================================
%% Internal functions
%%====================================================================

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
    {ok, {200, #{}, Compressed}};

fixture(<<?TEST_REPO_URI, "/versions">>, _) ->
    Versions = #{
        packages => [
            #{name => <<"ecto">>, versions => [<<"1.0.0">>]}
        ]
    },
    Payload = hex_registry:encode_versions(Versions),
    Signed = hex_registry:sign_protobuf(Payload, ?PRIVATE_KEY),
    Compressed = zlib:gzip(Signed),
    {ok, {200, #{}, Compressed}};

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
    {ok, {200, #{}, Compressed}};

fixture(<<?TEST_REPO_URI, "/tarballs/ecto-1.0.0.tar">>, #{<<"if-none-match">> := <<"\"dummy\"">> = ETag}) ->
    Headers = #{
      <<"etag">> => ETag
    },
    {ok, {304, Headers, <<"">>}};

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
    {ok, {200, [], term_to_binary(Payload)}};

fixture(<<?TEST_API_URI, "/packages/ecto">>, _) ->
    Payload = #{
        <<"name">> => <<"ecto">>,
        <<"releases">> => []
    },
    {ok, {200, [], term_to_binary(Payload)}};

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
    {ok, {200, [], term_to_binary(Payload)}};

fixture(URI, _) ->
    error({no_fixture, URI}).
