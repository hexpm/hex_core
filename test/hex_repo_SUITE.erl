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
    [get_names_test, get_versions_test, get_package_test, get_tarball_test, get_docs_test, repo_org_not_set].

get_names_test(_Config) ->
    {ok, {200, _, #{repository := <<"hexpm">>, packages := Packages}}} = hex_repo:get_names(?CONFIG),
    [#{name := <<"ecto">>}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_versions_test(_Config) ->
    {ok, {200, _, #{repository := <<"hexpm">>, packages := Packages}}} = hex_repo:get_versions(?CONFIG),
    [#{name := <<"ecto">>, versions := _}] =
        lists:filter(fun(#{name := Name}) -> Name == <<"ecto">> end, Packages),
    ok.

get_package_test(_Config) ->
    {ok, {200, _, #{repository := <<"hexpm">>, releases := Releases}}} = hex_repo:get_package(?CONFIG, <<"ecto">>),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {ok, {403, _, _}} = hex_repo:get_package(?CONFIG, <<"nonexisting">>),
    ok.

get_tarball_test(_Config) ->
    {ok, {200, #{<<"etag">> := ETag}, Tarball}} = hex_repo:get_tarball(?CONFIG, <<"ecto">>, <<"1.0.0">>),
    {ok, _} = hex_tarball:unpack(Tarball, memory),

    {ok, {304, _, _}} = hex_repo:get_tarball(maps:put(http_etag, ETag, ?CONFIG), <<"ecto">>, <<"1.0.0">>),

    {ok, {403, _, _}} = hex_repo:get_tarball(?CONFIG, <<"ecto">>, <<"9.9.9">>),
    ok.

get_docs_test(_Config) ->
  {ok, {200, #{<<"etag">> := ETag}, Docs}} = hex_repo:get_docs(?CONFIG, <<"ecto">>, <<"1.0.0">>),
  {ok, _} = hex_tarball:unpack_docs(Docs, memory),

  {ok, {304, _, _}} = hex_repo:get_docs(maps:put(http_etag, ETag, ?CONFIG), <<"ecto">>, <<"1.0.0">>),

  {ok, {403, _, _}} = hex_repo:get_docs(?CONFIG, <<"ecto">>, <<"9.9.9">>),
  ok.

repo_org_not_set(_Config) ->
    Config = maps:remove(repo_organization, ?CONFIG),
    {ok, {200, _, #{repository := <<"hexpm">>, releases := Releases}}} = hex_repo:get_package(Config, <<"ecto">>),
    [#{version := <<"1.0.0">>}] =
        lists:filter(fun(#{version := Version}) -> Version == <<"1.0.0">> end, Releases),

    {ok, {403, _, _}} = hex_repo:get_package(Config, <<"nonexisting">>),
    ok.
