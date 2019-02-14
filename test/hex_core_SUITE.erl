-module(hex_core_SUITE).

-compile([export_all]).

-include("hex_core.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [version_test].

version_test(_Config) ->
  ?assertEqual(?HEX_CORE_VERSION, hex_core:version()).
