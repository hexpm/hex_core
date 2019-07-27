-module(hex_version).
-export([parse/1]).

-type major() :: non_neg_integer().
-type minor() :: non_neg_integer().
-type patch() :: non_neg_integer().
-type pre() :: [binary() | non_neg_integer()].
-type build() :: binary() | undefined.

-type version() :: #{
    major => major(),
    minor => minor(),
    patch => patch(),
    pre   => pre(),
    build => build()
}.

-spec parse(binary()) -> {ok, version()} | {error, invalid_version}.
parse(Str) ->
  hex_verl:parse(Str).
