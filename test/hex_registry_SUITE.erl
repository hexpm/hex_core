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
    Packages = [
        #{name => <<"foo">>},
        #{name => <<"bar">>}
    ],
    Names = #{
        repository => <<"hexpm">>,
        packages => Packages
    },
    Payload = hex_registry:encode_names(Names),
    ?assertMatch({ok, Packages}, hex_registry:decode_names(Payload, <<"hexpm">>)),
    ?assertMatch({ok, Packages}, hex_registry:decode_names(Payload, no_verify)),
    ?assertMatch({error, unverified}, hex_registry:decode_names(Payload, <<"other_repo">>)),
    ok.

versions_test(_Config) ->
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
    Payload = hex_registry:encode_versions(Versions),
    ?assertMatch({ok, Packages}, hex_registry:decode_versions(Payload, <<"hexpm">>)),
    ?assertMatch({ok, Packages}, hex_registry:decode_versions(Payload, no_verify)),
    ?assertMatch({error, unverified}, hex_registry:decode_versions(Payload, <<"other_repo">>)),
    ok.

package_test(_Config) ->
    Releases = [
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
    ],
    Package = #{
        name => <<"foobar">>,
        repository => <<"hexpm">>,
        releases => Releases
    },
    Payload = hex_registry:encode_package(Package),
    ?assertMatch({ok, Releases}, hex_registry:decode_package(Payload, <<"hexpm">>, <<"foobar">>)),
    ?assertMatch({ok, Releases}, hex_registry:decode_package(Payload, no_verify, no_verify)),
    ?assertMatch({error, unverified}, hex_registry:decode_package(Payload, <<"other_repo">>, <<"foobar">>)),
    ?assertMatch({error, unverified}, hex_registry:decode_package(Payload, <<"hexpm">>, <<"other_package">>)),
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
    {ok, []} = hex_registry:decode_names(Payload, <<"hexpm">>),

    {error, bad_key} = hex_registry:decode_and_verify_signed(Signed, <<"bad">>),

    {error, unverified} = hex_registry:decode_and_verify_signed(Signed, HexpmPublicKey),

    ok.
