-module(hex_registry_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

names_test() ->
    Names = #{
        packages => [
            #{name => <<"foo">>, repository => <<"hexpm">>},
            #{name => <<"bar">>}
        ]
    },
    Payload = hex_registry:encode_names(Names),
    Names = hex_registry:decode_names(Payload),
    ok.

versions_test() ->
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
                retired => []
            }
        ]
    },
    Payload = hex_registry:encode_versions(Versions),
    Versions = hex_registry:decode_versions(Payload),
    ok.

package_test() ->
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
                        requirement => <<"~> 1.0">>
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
    Package = hex_registry:decode_package(Payload),
    ok.

signed_test() ->
    TestPublicKey = read_fixture("test_pub.pem"),
    TestPrivateKey = read_fixture("test_priv.pem"),
    HexpmPublicKey = read_fixture("hexpm_pub.pem"),
    Names = #{packages => []},
    Payload = hex_registry:encode_names(Names),

    Signed = hex_registry:sign_protobuf(Payload, TestPrivateKey),
    #{payload := Payload} = hex_registry:decode_signed(Signed),
    {ok, Payload} = hex_registry:decode_and_verify_signed(Signed, TestPublicKey),
    Names = hex_registry:decode_names(Payload),

    {error, bad_key} = hex_registry:decode_and_verify_signed(Signed, <<"bad">>),

    {error, unverified} = hex_registry:decode_and_verify_signed(Signed, HexpmPublicKey),

    ok.

read_fixture(Path) when is_list(Path) ->
    {ok, Binary} = file:read_file("test/fixtures/" ++ Path),
    Binary.
