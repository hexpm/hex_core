-module(prop_hex_tarball).
-include_lib("proper/include/proper.hrl").

prop_symmetric() ->
    ?FORALL(
        Binary,
        binary(),
        begin
            zlib:gunzip(hex_tarball:gzip(Binary)) =:= Binary
        end
    ).

prop_equivalent() ->
    ?FORALL(
        Binary,
        binary(),
        begin
            <<31, 139, 8, 0, 0, 0, 0, 0, 0, _Os, ZlibRest/binary>> = zlib:gzip(Binary),
            <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0, HexRest/binary>> = hex_tarball:gzip(Binary),
            ZlibRest =:= HexRest
        end
    ).

prop_metadata_strings() ->
    ?FORALL(
        Chars,
        list(oneof([$a, $\\, $", $', $\n, 16#E9])),
        begin
            Metadata = #{
                <<"name">> => <<"foo">>,
                <<"version">> => <<"1.0.0">>,
                <<"description">> => unicode:characters_to_binary(Chars),
                <<"chars">> => Chars
            },
            {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),
            {ok, #{metadata := Decoded}} = hex_tarball:unpack(Tarball, none),
            Decoded =:= Metadata
        end
    ).
