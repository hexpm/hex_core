-module(hex_http_test).
-behaviour(hex_http).
-export([get/2, user_agent_string/0]).
-define(TEST_REPO_URI, "https://repo.test").
-define(PRIVATE_KEY, element(2, file:read_file("test/fixtures/test_priv.pem"))).
-define(PUBLIC_KEY, element(2, file:read_file("test/fixtures/test_pub.pem"))).

%%====================================================================
%% API functions
%%====================================================================

get(URI, _Headers) ->
    fixture(URI).

user_agent_string() ->
    "(test)".

%%====================================================================
%% Internal functions
%%====================================================================

fixture(?TEST_REPO_URI ++ "/names") ->
    Names = #{
        packages => [
            #{name => <<"ecto">>}
        ]
    },
    Payload = hex_registry:encode_names(Names),
    Signed = hex_registry:sign_protobuf(Payload, ?PRIVATE_KEY),
    Compressed = zlib:gzip(Signed),
    {ok, {200, [], Compressed}};

fixture(?TEST_REPO_URI ++ "/versions") ->
    Versions = #{
        packages => [
            #{name => <<"ecto">>, versions => [<<"1.0.0">>]}
        ]
    },
    Payload = hex_registry:encode_versions(Versions),
    Signed = hex_registry:sign_protobuf(Payload, ?PRIVATE_KEY),
    Compressed = zlib:gzip(Signed),
    {ok, {200, [], Compressed}};

fixture(?TEST_REPO_URI ++ "/packages/ecto") ->
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
    {ok, {200, [], Compressed}};

fixture(?TEST_REPO_URI ++ "/tarballs/ecto-1.0.0.tar") ->
    {ok, {Tarball, _Checksum}} = hex_tarball:create(#{<<"name">> => <<"ecto">>}, []),
    {ok, {200, [], Tarball}};

fixture(?TEST_REPO_URI ++ _) ->
    {ok, {403, [], <<"not found">>}};

fixture(URI) ->
    error({no_fixture, URI}).
