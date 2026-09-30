-module(prop_hex_deflate).
-include_lib("proper/include/proper.hrl").

prop_round_trip() ->
    ?FORALL(
        Binary,
        binary(),
        begin
            zlib:unzip(hex_deflate:compress(Binary)) =:= Binary
        end
    ).

prop_repetitive_round_trip() ->
    ?FORALL(
        {Chunks, Repeats},
        {list(binary()), pos_integer()},
        begin
            Binary = iolist_to_binary(lists:duplicate(Repeats, Chunks)),
            zlib:unzip(hex_deflate:compress(Binary)) =:= Binary
        end
    ).

prop_chunked_round_trip() ->
    ?FORALL(
        {Binary, Chunk},
        {binary(), range(1, 300)},
        begin
            zlib:unzip(hex_deflate:compress(Binary, #{chunk => Chunk})) =:= Binary
        end
    ).
