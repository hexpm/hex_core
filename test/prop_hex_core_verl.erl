%% Vendored from verl v1.0.2, do not edit manually

-module(prop_hex_core_verl).
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_basic_valid_semver0() ->
    ?FORALL({Maj, Min, P, Pre},{non_neg_integer(), non_neg_integer(), non_neg_integer(), non_empty(binary())},
            begin
                Major = integer_to_binary(Maj),
                Minor = integer_to_binary(Min),
                Patch = integer_to_binary(P),
                V = <<Major/binary, <<".">>/binary,  Minor/binary, <<".">>/binary,  Patch/binary, <<"-">>/binary, Pre/binary >>,
                case re:run(Pre, "^[0-9A-Za-z-]+$") of
                    nomatch ->
                        {error, invalid_version} =:= hex_core_verl:parse(V);
                    _ ->
                        {ok, Parsed} = hex_core_verl:parse(V),
                        PreEl = case re:run(Pre, "^[0-9]+$") of
                                    nomatch ->
                                        [Pre];
                                    _ ->
                                        [binary_to_integer(Pre)]
                                end,
                        Exp = #{build => undefined, major => Maj, minor => Min, patch => P, pre => PreEl},
                        Exp =:= Parsed
                end
            end).

prop_basic_valid_semver() ->
    ?FORALL({Maj, Min, P},{non_neg_integer(), non_neg_integer(), non_neg_integer()},
            begin
                Major = integer_to_binary(Maj),
                Minor = integer_to_binary(Min),
                Patch = integer_to_binary(P),
                V = <<Major/binary, <<".">>/binary,  Minor/binary, <<".">>/binary,  Patch/binary>>,
                {ok, Parsed} = hex_core_verl:parse(V),
                Exp = #{build => undefined, major => Maj, minor => Min, patch => P, pre => []},
                Exp =:= Parsed
            end).

prop_basic_invalid_semver() ->
    ?FORALL({Maj, Min, P},{neg_integer(), neg_integer(), neg_integer()},
            begin
                Major = integer_to_binary(Maj),
                Minor = integer_to_binary(Min),
                Patch = integer_to_binary(P),
                V = <<Major/binary, <<".">>/binary,  Minor/binary, <<".">>/binary,  Patch/binary>>,
                {error, invalid_version} =:= hex_core_verl:parse(V)
            end).

prop_basic_invalid_semver_more() ->
    ?FORALL({Maj, Min, P},{any(), any(), any()},
            begin
                Major = term_to_binary(Maj),
                Minor = term_to_binary(Min),
                Patch = term_to_binary(P),
                V = <<Major/binary, <<".">>/binary,  Minor/binary, <<".">>/binary,  Patch/binary>>,
                {error, invalid_version} =:= hex_core_verl:parse(V)
            end).


prop_basic_invalid_semver_more2() ->
    ?FORALL({Maj, Min, P, Pre},{binary(), binary(), binary(), binary()},
            begin
                V = <<Maj/binary, <<".">>/binary,  Min/binary, <<".">>/binary,  P/binary, <<"-">>/binary, Pre/binary>>,
                {error, invalid_version} =:= hex_core_verl:parse(V)
            end).
%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
