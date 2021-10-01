-module(hex_licenses_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [licenses_test, invalid_licenses_test].

licenses_test(_Config) ->
    hex_licenses:licenses(),
    ok.

invalid_licenses_test(_Config) ->
    [] = hex_licenses:invalid_licenses([<<"MIT">>]),
    ok.
