-module(prop_hex_tarball).
-include_lib("proper/include/proper.hrl").

prop_symmetric() ->
    ?FORALL(Binary, binary(),
        begin
            zlib:gunzip(hex_tarball:gzip(Binary)) =:= Binary
        end).

prop_equivalent() ->
    ?FORALL(Binary, binary(),
        begin
            <<31, 139, 8, 0, 0, 0, 0, 0, 0, _Os, ZlibRest/binary>> = zlib:gzip(Binary),
            <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0, HexRest/binary>> = hex_tarball:gzip(Binary),
            ZlibRest =:= HexRest
        end).
