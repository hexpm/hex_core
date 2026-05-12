-module(hex_licenses_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [license_test, license_ref_test, invalid_license_test].

license_test(_Config) ->
    true = hex_licenses:valid(<<"MIT">>),
    ok.

license_ref_test(_Config) ->
    true = hex_licenses:valid(<<"LicenseRef-Journey">>),
    true = hex_licenses:valid(<<"LicenseRef-acme.1-2">>),
    false = hex_licenses:valid(<<"LicenseRef-">>),
    false = hex_licenses:valid(<<"LicenseRef-Journey License">>),
    false = hex_licenses:valid(<<"LicenseRef-Journey_License">>),
    ok.

invalid_license_test(_Config) ->
    false = hex_licenses:valid(<<"MICROSOFT">>),
    ok.
