-module(hex_licenses_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [license_test, invalid_license_test].

license_test(_Config) ->
    true = hex_licenses:valid([<<"MIT">>]),
    ok.

invalid_license_test(_Config) ->
    false = hex_licenses:valid([<<"MICROSOFT">>]),
    ok.
