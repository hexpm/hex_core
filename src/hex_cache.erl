-module(hex_cache).
-export([
    write_tarball/3,
    load_from_cache/3
]).

%%====================================================================
%% API functions
%%====================================================================

write_tarball(CacheDir, Filename, Data) ->
    Path = filename:join(CacheDir, Filename),
    file:write_file(Path, Data).

load_from_cache(_Name, _Version, undefined) ->
    {error, no_cache_dir};
load_from_cache(Name, Version, Path) ->
    N = hex_util:tarball_name(Name, Version),
    F = filename:join(Path, N),
    {ok, Data} = file:read_file(F),
    Data.

%%====================================================================
%% Internal functions
%%====================================================================

