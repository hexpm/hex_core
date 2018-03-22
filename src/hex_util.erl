-module(hex_util).
-export([
         tarball_name/2
]).

tarball_name(Name, Version) ->
    Name ++ "-" ++ Version ++ ".tar".
