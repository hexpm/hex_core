-module(hex_registry_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{require, {ssl_certs, [test_pub, test_priv, hexpm_pub]}}].

all() ->
    [names_test, versions_test, package_test, signed_test].

names_test(_Config) ->
    TestPublicKey = ct:get_config({ssl_certs, test_pub}),
    TestPrivateKey = ct:get_config({ssl_certs, test_priv}),
    Packages = [
        #{name => <<"foo">>, updated_at => #{seconds => 0, nanos => 0}},
        #{name => <<"bar">>, updated_at => #{seconds => 1619818073, nanos => 999}}
    ],
    Names = #{
        repository => <<"hexpm">>,
        packages => Packages
    },
    Payload = hex_registry:build_names(Names, TestPrivateKey),
    ?assertMatch({ok, Names}, hex_registry:unpack_names(Payload, <<"hexpm">>, TestPublicKey)),
    ?assertMatch({ok, Names}, hex_registry:unpack_names(Payload, no_verify, TestPublicKey)),
    ?assertMatch(
        {error, bad_repo_name}, hex_registry:unpack_names(Payload, <<"other_repo">>, TestPublicKey)
    ),
    ok.

versions_test(_Config) ->
    TestPublicKey = ct:get_config({ssl_certs, test_pub}),
    TestPrivateKey = ct:get_config({ssl_certs, test_priv}),
    Packages = [
        #{
            name => <<"foo">>,
            versions => [<<"1.0.0-rc.1">>, <<"1.1.0-rc.2">>, <<"1.1.0">>],
            retired => [0, 1]
        },
        #{
            name => <<"bar">>,
            versions => [<<"1.0.0">>],
            retired => []
        }
    ],
    Versions = #{
        repository => <<"hexpm">>,
        packages => Packages
    },
    Payload = hex_registry:build_versions(Versions, TestPrivateKey),
    ?assertMatch({ok, Versions}, hex_registry:unpack_versions(Payload, <<"hexpm">>, TestPublicKey)),
    ?assertMatch({ok, Versions}, hex_registry:unpack_versions(Payload, no_verify, TestPublicKey)),
    ?assertMatch(
        {error, bad_repo_name},
        hex_registry:unpack_versions(Payload, <<"other_repo">>, TestPublicKey)
    ),
    ok.

package_test(_Config) ->
    TestPublicKey = ct:get_config({ssl_certs, test_pub}),
    TestPrivateKey = ct:get_config({ssl_certs, test_priv}),
    Releases = [
        #{
            version => <<"1.0.0">>,
            inner_checksum => <<"some checksum">>,
            outer_checksum => <<"some checksum">>,
            dependencies => [
                #{
                    package => <<"foo">>,
                    requirement => <<"~> 1.0">>,
                    optional => false,
                    app => <<"foo_app">>,
                    repository => <<"hexpm">>
                },
                #{
                    package => <<"bar">>,
                    requirement => <<"~> 1.0">>,
                    optional => false,
                    app => <<>>,
                    repository => <<>>
                }
            ],
            retired => #{
                reason => 'RETIRED_SECURITY',
                message => <<"CVE-XXXX">>
            }
        }
    ],
    Package = #{
        name => <<"foobar">>,
        repository => <<"hexpm">>,
        releases => Releases
    },
    Payload = hex_registry:build_package(Package, TestPrivateKey),
    ?assertMatch(
        {ok, Package},
        hex_registry:unpack_package(Payload, <<"hexpm">>, <<"foobar">>, TestPublicKey)
    ),
    ?assertMatch(
        {ok, Package}, hex_registry:unpack_package(Payload, no_verify, no_verify, TestPublicKey)
    ),
    ?assertMatch(
        {error, bad_repo_name},
        hex_registry:unpack_package(Payload, <<"other_repo">>, <<"foobar">>, TestPublicKey)
    ),
    ?assertMatch(
        {error, bad_repo_name},
        hex_registry:unpack_package(Payload, <<"hexpm">>, <<"other_package">>, TestPublicKey)
    ),
    ok.

signed_test(_Config) ->
    TestPublicKey = ct:get_config({ssl_certs, test_pub}),
    TestPrivateKey = ct:get_config({ssl_certs, test_priv}),
    HexpmPublicKey = ct:get_config({ssl_certs, hexpm_pub}),

    Names = #{repository => <<"hexpm">>, packages => []},
    Payload = hex_registry:encode_names(Names),

    Signed = hex_registry:sign_protobuf(Payload, TestPrivateKey),
    #{payload := Payload} = hex_registry:decode_signed(Signed),
    {ok, Payload} = hex_registry:decode_and_verify_signed(Signed, TestPublicKey),
    {ok, #{packages := []}} = hex_registry:decode_names(Payload, <<"hexpm">>),

    {error, bad_key} = hex_registry:decode_and_verify_signed(Signed, <<"bad">>),

    {error, bad_signature} = hex_registry:decode_and_verify_signed(Signed, HexpmPublicKey),

    ok.
