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
    Names = #{
        packages => [
            #{name => <<"foo">>, repository => <<"hexpm">>},
            #{name => <<"bar">>, repository => <<>>}
        ]
    },
    Payload = hex_registry:encode_names(Names),
    ?assertMatch(Names, hex_registry:decode_names(Payload)),
    ok.

versions_test(_Config) ->
    Versions = #{
        packages => [
            #{
                name => <<"foo">>,
                versions => [<<"1.0.0-rc.1">>, <<"1.1.0-rc.2">>, <<"1.1.0">>],
                retired => [0, 1],
                repository => <<"hexpm">>
            },
            #{
                name => <<"bar">>,
                versions => [<<"1.0.0">>],
                retired => [],
                repository => <<>>
            }
        ]
    },
    Payload = hex_registry:encode_versions(Versions),
    ?assertMatch(Versions, hex_registry:decode_versions(Payload)),
    ok.

package_test(_Config) ->
    Package = #{
        releases => [
            #{
                version => <<"1.0.0">>,
                checksum => <<"some checksum">>,
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
        ]
    },
    Payload = hex_registry:encode_package(Package),
    ?assertMatch(Package, hex_registry:decode_package(Payload)),
    ok.

signed_test(_Config) ->
    TestPublicKey = ct:get_config({ssl_certs, test_pub}),
    TestPrivateKey = ct:get_config({ssl_certs, test_priv}),
    HexpmPublicKey = ct:get_config({ssl_certs, hexpm_pub}),

    Names = #{packages => []},
    Payload = hex_registry:encode_names(Names),

    Signed = hex_registry:sign_protobuf(Payload, TestPrivateKey),
    #{payload := Payload} = hex_registry:decode_signed(Signed),
    {ok, Payload} = hex_registry:decode_and_verify_signed(Signed, TestPublicKey),
    Names = hex_registry:decode_names(Payload),

    {error, bad_key} = hex_registry:decode_and_verify_signed(Signed, <<"bad">>),

    {error, unverified} = hex_registry:decode_and_verify_signed(Signed, HexpmPublicKey),

    ok.

