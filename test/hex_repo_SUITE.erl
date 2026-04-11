-module(hex_repo_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(DEFAULT_HTTP_ADAPTER_CONFIG, #{profile => default}).

-define(CONFIG, (hex_core:default_config())#{
    http_adapter => {hex_http_test, ?DEFAULT_HTTP_ADAPTER_CONFIG},
    http_user_agent_fragment => <<"(test)">>,
    repo_url => <<"https://repo.test">>,
    repo_public_key => ct:get_config({ssl_certs, test_pub})
}).
% -define(CONFIG, hex_core:default_config()).

suite() ->
    [{require, {ssl_certs, test_pub}}].

all() ->
    [
        get_names_test,
        get_versions_test,
        get_package_test,
        get_tarball_test,
        get_tarball_to_file_test,
        get_docs_test,
        get_docs_to_file_test,
        get_hex_installs_test,
        get_public_key_test,
        repo_org_not_set,
        fingerprint_test,
        fingerprint_equal_test
    ].

get_names_test(_Config) ->
    {ok, {200, _, #{repository := <<"hexpm">>, packages := Packages}}} = hex_repo:get_names(
        ?CONFIG
    ),
    [#{name := <<"ecto">>}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_versions_test(_Config) ->
    {ok, {200, _, #{repository := <<"hexpm">>, packages := Packages}}} = hex_repo:get_versions(
        ?CONFIG
    ),
    [#{name := <<"ecto">>, versions := _}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_package_test(_Config) ->
    {ok, {200, _, #{repository := <<"hexpm">>, releases := Releases}}} = hex_repo:get_package(
        ?CONFIG, <<"ecto">>
    ),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {ok, {403, _, _}} = hex_repo:get_package(?CONFIG, <<"nonexisting">>),
    ok.

get_tarball_test(_Config) ->
    {ok, {200, #{<<"etag">> := ETag}, Tarball}} = hex_repo:get_tarball(
        ?CONFIG, <<"ecto">>, <<"1.0.0">>
    ),
    {ok, _} = hex_tarball:unpack(Tarball, memory),

    {ok, {304, _, _}} = hex_repo:get_tarball(
        maps:put(http_etag, ETag, ?CONFIG), <<"ecto">>, <<"1.0.0">>
    ),

    {ok, {403, _, _}} = hex_repo:get_tarball(?CONFIG, <<"ecto">>, <<"9.9.9">>),
    ok.

get_tarball_to_file_test(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Filename = filename:join(PrivDir, "ecto-1.0.0.tar"),
    {ok, {200, #{<<"etag">> := _}}} = hex_repo:get_tarball_to_file(
        ?CONFIG, <<"ecto">>, <<"1.0.0">>, Filename
    ),
    {ok, Tarball} = file:read_file(Filename),
    {ok, _} = hex_tarball:unpack(Tarball, memory),

    {ok, {403, _}} = hex_repo:get_tarball_to_file(?CONFIG, <<"ecto">>, <<"9.9.9">>, Filename),
    ok.

get_docs_test(_Config) ->
    {ok, {200, #{<<"etag">> := ETag}, Docs}} = hex_repo:get_docs(?CONFIG, <<"ecto">>, <<"1.0.0">>),
    {ok, _} = hex_tarball:unpack_docs(Docs, memory),

    {ok, {304, _, _}} = hex_repo:get_docs(
        maps:put(http_etag, ETag, ?CONFIG), <<"ecto">>, <<"1.0.0">>
    ),

    {ok, {403, _, _}} = hex_repo:get_docs(?CONFIG, <<"ecto">>, <<"9.9.9">>),
    ok.

get_docs_to_file_test(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Filename = filename:join(PrivDir, "ecto-1.0.0-docs.tar.gz"),
    {ok, {200, #{<<"etag">> := _}}} = hex_repo:get_docs_to_file(
        ?CONFIG, <<"ecto">>, <<"1.0.0">>, Filename
    ),
    {ok, Docs} = file:read_file(Filename),
    {ok, _} = hex_tarball:unpack_docs(Docs, memory),

    {ok, {403, _}} = hex_repo:get_docs_to_file(?CONFIG, <<"ecto">>, <<"9.9.9">>, Filename),
    ok.

get_hex_installs_test(_Config) ->
    {ok, {200, _, CSV}} = hex_repo:get_hex_installs(?CONFIG),
    ?assert(is_binary(CSV)),
    ?assert(binary:match(CSV, <<"1.0.0,abc123,1.13.0\n">>) =/= nomatch),
    ok.

get_public_key_test(_Config) ->
    {ok, {200, #{<<"etag">> := ETag}, PublicKey}} = hex_repo:get_public_key(?CONFIG),
    [{'SubjectPublicKeyInfo', _, not_encrypted}] = public_key:pem_decode(PublicKey),

    {ok, {304, _, _}} = hex_repo:get_public_key(maps:put(http_etag, ETag, ?CONFIG)),

    {ok, {403, _, _}} = hex_repo:get_public_key(
        maps:put(repo_url, <<"https://repo.test/not_found">>, ?CONFIG)
    ),
    ok.

repo_org_not_set(_Config) ->
    Config = maps:remove(repo_organization, ?CONFIG),
    {ok, {200, _, #{repository := <<"hexpm">>, releases := Releases}}} = hex_repo:get_package(
        Config, <<"ecto">>
    ),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {ok, {403, _, _}} = hex_repo:get_package(Config, <<"nonexisting">>),
    ok.

fingerprint_test(_Config) ->
    PublicKeyPem = <<
        "-----BEGIN PUBLIC KEY-----\n"
        "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB\n"
        "Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+\n"
        "IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM\n"
        "3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW\n"
        "XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ\n"
        "J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4\n"
        "0wIDAQAB\n"
        "-----END PUBLIC KEY-----\n"
    >>,
    ExpectedFingerprint = "SHA256:O1LOYhHFW4kcrblKAxROaDEzLD8bn1seWbe5tq8TRsk",
    ?assertEqual(ExpectedFingerprint, hex_repo:fingerprint(PublicKeyPem)),
    ok.

fingerprint_equal_test(_Config) ->
    PublicKeyPem = <<
        "-----BEGIN PUBLIC KEY-----\n"
        "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB\n"
        "Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+\n"
        "IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM\n"
        "3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW\n"
        "XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ\n"
        "J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4\n"
        "0wIDAQAB\n"
        "-----END PUBLIC KEY-----\n"
    >>,
    CorrectFingerprint = "SHA256:O1LOYhHFW4kcrblKAxROaDEzLD8bn1seWbe5tq8TRsk",
    WrongFingerprint = "SHA256:WrongFingerprint123456789012345678901234567",
    ?assert(hex_repo:fingerprint_equal(PublicKeyPem, CorrectFingerprint)),
    ?assertNot(hex_repo:fingerprint_equal(PublicKeyPem, WrongFingerprint)),
    ok.
