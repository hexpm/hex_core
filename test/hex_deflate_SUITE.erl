-module(hex_deflate_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        empty_input_test,
        small_inputs_test,
        generated_inputs_test,
        options_test,
        chunk_boundaries_test,
        iodata_test,
        budget_test,
        max_chunk_test,
        skip_test,
        invalid_chunk_test,
        worker_error_test,
        worker_exit_test,
        huffman_length_limit_test,
        huffman_reference_test,
        literal_depth_test,
        deterministic_output_test,
        golden_output_test
    ].

empty_input_test(_Config) ->
    ?assertEqual(<<>>, zlib:unzip(hex_deflate:compress(<<>>))).

small_inputs_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(
        fun(Size) ->
            [assert_round_trip(gen(Generator, Size), #{}) || Generator <- lists:seq(1, 7)]
        end,
        lists:seq(0, 300)
    ).

generated_inputs_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(
        fun(Size) ->
            [assert_round_trip(gen(Generator, Size), #{}) || Generator <- lists:seq(1, 7)]
        end,
        [1000, 32767, 32768, 32769, 70000, 131073, 131075, 300000]
    ).

%% Random inputs from every generator with random options
options_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(
        fun(_) ->
            Size =
                case rand:uniform(4) of
                    1 -> rand:uniform(64) - 1;
                    2 -> rand:uniform(2000);
                    3 -> rand:uniform(70000);
                    4 -> rand:uniform(300000)
                end,
            {BlockSyms, Chunk} =
                if
                    Size > 20000 ->
                        {pick([100, 1000, 16384, 65536]), pick([4096, 32768, 65536, 131072])};
                    true ->
                        {pick([1, 2, 3, 7, 100, 16384]), pick([1, 4, 7, 100, 4096])}
                end,
            Opts = #{
                lazy => rand:uniform(2) =:= 1,
                chain => pick([0, 1, 2, 4, 8, 32, 128, 1024]),
                nice => pick([3, 4, 8, 16, 128, 258]),
                good => pick([3, 4, 8, 32]),
                max_lazy => pick([3, 4, 16, 258]),
                block_syms => BlockSyms,
                hash_bits => pick([8, 12, 15, 16]),
                chunk => Chunk,
                workers => pick([1, 2, 4]),
                budget => pick([0, 1, 4, 16, 1 bsl 20]),
                skip => pick([0, 1, 16, 256, 1 bsl 58]),
                stride => pick([1, 2, 4, 64])
            },
            assert_round_trip(gen(rand:uniform(7), Size), Opts)
        end,
        lists:seq(1, 200)
    ).

%% Chunks of sizes around the minimum match, maximum match and history
%% lengths, with stored blocks (random input) starting at every bit offset
chunk_boundaries_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(
        fun(Chunk) ->
            Size = min(100000, 20 * Chunk + 2000),
            Bin = iolist_to_binary([
                gen(rand:uniform(7), rand:uniform(Size div 20))
             || _ <- lists:seq(1, 40)
            ]),
            assert_round_trip(Bin, #{chunk => Chunk})
        end,
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 257, 258, 259, 4095, 32767, 32768, 32769]
    ).

iodata_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    IoData = [gen(7, 1000), [<<"abc">>, $d | gen(4, 5000)], [], gen(1, 3000)],
    ?assertEqual(
        hex_deflate:compress(iolist_to_binary(IoData)), hex_deflate:compress(IoData)
    ),
    ?assertEqual(iolist_to_binary(IoData), zlib:unzip(hex_deflate:compress(IoData))).

%% Chain step budgets, from none to unlimited, on inputs with long chains
budget_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    Acgt = <<<<(element(rand:uniform(4), {$a, $c, $g, $t}))>> || _ <- lists:seq(1, 200000)>>,
    Counter = <<<<I:32>> || I <- lists:seq(1, 50000)>>,
    [
        assert_round_trip(Bin, #{budget => Budget, lazy => Lazy})
     || Bin <- [Acgt, Counter], Budget <- [0, 1, 16, 1 bsl 20], Lazy <- [true, false]
    ].

%% The largest chunk, whose indexes come closest to the one marking
%% positions without a previous position
max_chunk_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    Bin = iolist_to_binary([gen(G, 200000) || G <- [4, 7, 2, 6, 5, 1, 3]]),
    [assert_round_trip(Bin, #{chunk => 7 bsl 16, lazy => Lazy}) || Lazy <- [true, false]].

%% Sparse searches on incompressible input followed by compressible input,
%% and strides that are not powers of two
skip_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    Bin = iolist_to_binary([gen(1, 100000), gen(7, 100000), gen(1, 50000), gen(4, 50000)]),
    [
        assert_round_trip(Bin, #{skip => Skip, stride => Stride, lazy => Lazy})
     || Skip <- [0, 1, 64, 256], Stride <- [1, 2, 4, 1 bsl 20], Lazy <- [true, false]
    ],
    %% Searches resume on the compressible parts.
    Full = byte_size(hex_deflate:compress(Bin, #{stride => 1})),
    Sparse = byte_size(hex_deflate:compress(Bin, #{skip => 64, stride => 4})),
    ?assert(Sparse * 100 < Full * 101),
    ?assertError({badarg, {stride, 3}}, hex_deflate:compress(Bin, #{stride => 3})),
    ?assertError({badarg, {stride, 0}}, hex_deflate:compress(Bin, #{stride => 0})).

invalid_chunk_test(_Config) ->
    ?assertError({badarg, {chunk, 0}}, hex_deflate:compress(<<"abc">>, #{chunk => 0})),
    ?assertError(
        {badarg, {chunk, (7 bsl 16) + 1}},
        hex_deflate:compress(<<"abc">>, #{chunk => (7 bsl 16) + 1})
    ).

%% An exception in a worker is raised in the caller, after the other workers
%% are stopped, without messages or monitors left behind
worker_error_test(_Config) ->
    Self = self(),
    F = fun
        (2) ->
            Self ! {worker, self()},
            error(boom);
        (_) ->
            Self ! {worker, self()},
            receive
            after infinity -> ok
            end
    end,
    Caller = spawn(fun() ->
        Result =
            try
                hex_deflate:parallel(F, 8, 4, [])
            catch
                Class:Reason:Stack -> {Class, Reason, hd(Stack)}
            end,
        Self ! {result, Result, process_info(self(), [messages, monitors])}
    end),
    Result =
        receive
            {result, R, Info} -> {R, Info}
        end,
    ?assertMatch(
        {{error, boom, {?MODULE, _, _, _}}, [{messages, []}, {monitors, []}]}, Result
    ),
    Workers = workers(),
    ?assertEqual(4, length(Workers)),
    ?assertEqual([], [P || P <- [Caller | Workers], is_process_alive(P)]).

%% A worker that is killed takes the caller down with it
worker_exit_test(_Config) ->
    Self = self(),
    F = fun
        (1) ->
            exit(self(), kill);
        (_) ->
            Self ! {worker, self()},
            receive
            after infinity -> ok
            end
    end,
    {Caller, MRef} = spawn_monitor(fun() -> hex_deflate:parallel(F, 8, 4, []) end),
    ?assertEqual(
        killed,
        receive
            {'DOWN', MRef, process, Caller, Reason} -> Reason
        end
    ),
    Workers = workers(),
    ?assertEqual(3, length(Workers)),
    ?assertEqual([], [P || P <- Workers, is_process_alive(P)]).

%% Fibonacci frequencies force Huffman trees deeper than 15 bits (literal and
%% length codes) and 7 bits (code length codes)
huffman_length_limit_test(_Config) ->
    Fib = fib(30),
    {LitLengths, LitCounts} = huff_lengths(Fib ++ lists:duplicate(286 - 30, 0), 15),
    {CodeLengths, CodeCounts} = huff_lengths(lists:sublist(Fib, 19), 7),
    ?assertEqual({15, true}, kraft(LitLengths, 15)),
    ?assertEqual({7, true}, kraft(CodeLengths, 7)),
    ?assertEqual(LitCounts, length_counts(LitLengths, 15)),
    ?assertEqual(CodeCounts, length_counts(CodeLengths, 7)).

%% Code lengths match a Huffman tree built from nodes and lists with the
%% two-queue method (ties prefer leaves), depths limited as in miniz
huffman_reference_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(
        fun(_) ->
            N = 1 + rand:uniform(285),
            MaxBits =
                if
                    N > 128 -> 15;
                    true -> pick([7, 15])
                end,
            Freqs =
                case rand:uniform(4) of
                    1 -> [rand:uniform(3) - 1 || _ <- lists:seq(1, N)];
                    2 -> [rand:uniform(1 bsl 20) - 1 || _ <- lists:seq(1, N)];
                    3 -> shuffle(lists:sublist(fib(min(N, 60)) ++ lists:duplicate(N, 0), N));
                    4 -> [max(0, rand:uniform(40) - 30) || _ <- lists:seq(1, N)]
                end,
            ?assertEqual(
                {Freqs, ref_lengths(Freqs, MaxBits)}, {Freqs, huff_lengths(Freqs, MaxBits)}
            )
        end,
        lists:seq(1, 3000)
    ).

%% Literal-only blocks whose byte counts are Fibonacci numbers
literal_depth_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    Fib = fib(25),
    Bytes = iolist_to_binary([binary:copy(<<I>>, F) || {I, F} <- lists:zip(lists:seq(0, 24), Fib)]),
    Shuffled = <<<<B>> || {_, B} <- lists:sort([{rand:uniform(), B} || <<B>> <= Bytes])>>,
    assert_round_trip(Shuffled, #{chain => 0, block_syms => 1 bsl 30}).

%% The output does not depend on the number of workers
deterministic_output_test(_Config) ->
    rand:seed(exsss, {1, 2, 3}),
    Bin = iolist_to_binary([gen(G, 20000) || G <- lists:seq(1, 7)]),
    Expected = hex_deflate:compress(Bin, #{workers => 1, chunk => 5000}),
    [
        ?assertEqual(Expected, hex_deflate:compress(Bin, #{workers => W, chunk => 5000}))
     || W <- [2, 3, 8, 64]
    ],
    ?assertEqual(hex_deflate:compress(Bin, #{workers => 1}), hex_deflate:compress(Bin)).

%% The exact output for inputs that exercise each part of the encoder: every
%% block type, lazy and greedy matching, runs of one and two bytes, sparse
%% searching, several chunks and non-default options. Package checksums
%% depend on this output, so it must not change on any OTP version.
golden_output_test(_Config) ->
    [
        begin
            Compressed = hex_deflate:compress(Input, Opts),
            ?assertEqual(
                {Name, Size, Sha256},
                {Name, byte_size(Compressed), binary:encode_hex(crypto:hash(sha256, Compressed))}
            ),
            ?assertEqual(Input, zlib:unzip(Compressed))
        end
     || {{Name, Input, Opts}, {Name, Size, Sha256}} <- lists:zip(golden_cases(), golden_outputs())
    ].

golden_cases() ->
    Text = golden_text(300000),
    Random = golden_bytes(200000),
    [
        {empty, <<>>, #{}},
        {one_byte, <<"a">>, #{}},
        {three_bytes, <<"abc">>, #{}},
        {short_text, binary:part(Text, 0, 1000), #{}},
        {text, Text, #{}},
        {zeros, binary:copy(<<0>>, 300000), #{}},
        {ab, binary:copy(<<"ab">>, 150000), #{}},
        {random, Random, #{}},
        {acgt, <<<<(element((B band 3) + 1, {$a, $c, $g, $t}))>> || <<B>> <= Random>>, #{}},
        {hex, binary:encode_hex(binary:part(Random, 0, 100000)), #{}},
        {counter, <<<<I:32>> || I <- lists:seq(0, 49999)>>, #{}},
        {greedy, Text, #{lazy => false}},
        {small_chunks, Text, #{
            chunk => 5000, stride => 1, skip => 0, hash_bits => 12, block_syms => 1000
        }},
        {low_budget, Text, #{budget => 1, chain => 4, nice => 8, good => 4, max_lazy => 4}}
    ].

%% {Name, Size, SHA-256} of the output for each of golden_cases/0.
golden_outputs() ->
    [
        {empty, 2, <<"9B4FB24EDD6D1D8830E272398263CDBF026B97392CC35387B991DC0248A628F9">>},
        {one_byte, 3, <<"59FCED9A2FF83DABDD7055CF3ABD6C12B802E6397B6FBA5B8251CDF95D2BC991">>},
        {three_bytes, 5, <<"7CE573D21753AEDBE7D6D1F5893E3F1265A35B9B51E42A154174A4CE9602AE39">>},
        {short_text, 380, <<"C62C4B585E2B5FE40795D46A71234A9F2955D2356CB2C40FA61758F8D060E467">>},
        {text, 71585, <<"C68042BAFAE458C6D52C3F52EFD828773BBE1C58910329CB3D6403369A7971BB">>},
        {zeros, 338, <<"BAA8ADC56F3F668DA08E8AB4AC05D05D422939F5B30BCCF4B217EB770C040066">>},
        {ab, 339, <<"501EA0BA195DCFCBF59B95707053ED64A44FE20B57B43ED6DDF9ECBF62BC7923">>},
        {random, 200065, <<"8EAA50E0C25DCE5C5F2B138965C2370744046105E175E2589E66E6B7FFD92338">>},
        {acgt, 60218, <<"A5C534C0FB65947AD08B8F56ED7191E43DB5F630C65EB794A7BFB4A5CD856180">>},
        {hex, 114313, <<"B95B0AB17CB95CE110E18B04CD8D296981695CE1D2E5946F0D2A7478117460A7">>},
        {counter, 99588, <<"668A27A99D694B2C04D9C10B50A8A8183BD14A09E97038507097E68B03CC5A70">>},
        {greedy, 71312, <<"A037082899B675BA5FD7011D7C80054FFBA3D72383E51A6FE636AE8F840D1FAC">>},
        {small_chunks, 73136,
            <<"88175DEE616AE3CFB33E936D20051D9425FA29140B1DC9E65F8BE90683FE4FE9">>},
        {low_budget, 88655, <<"FF293DEB27FF9CB3604AEF40EDDBD758FB385E8F1E1E5E04751B57826CD62A30">>}
    ].

%%====================================================================
%% Helpers
%%====================================================================

ref_lengths(Freqs, MaxBits) ->
    N = length(Freqs),
    Used = [{F, S} || {S, F} <- lists:zip(lists:seq(0, N - 1), Freqs), F > 0],
    Leaves = lists:sort(
        case Used of
            [] -> [{1, 0}, {1, 1}];
            [{F, 0}] -> [{1, 1}, {F, 0}];
            [{F, S}] -> [{1, 0}, {F, S}];
            _ -> Used
        end
    ),
    Tree = ref_build([{W, S} || {W, S} <- Leaves], []),
    Depths = ref_depths(Tree, 0),
    Clamped = [min(D, MaxBits) || {_, D} <- Depths],
    Counts0 = list_to_tuple([length([D || D <- Clamped, D =:= L]) || L <- lists:seq(1, MaxBits)]),
    Total = lists:sum([element(L, Counts0) bsl (MaxBits - L) || L <- lists:seq(1, MaxBits)]),
    Counts = ref_kraft(Counts0, Total, MaxBits),
    Order = lists:reverse(Leaves),
    Lens = ref_assign(Order, [{L, element(L, Counts)} || L <- lists:seq(1, MaxBits)]),
    {list_to_tuple([proplists:get_value(S, Lens, 0) || S <- lists:seq(0, N - 1)]), Counts}.

%% Leaves are {Weight, Symbol}, internal nodes {Weight, {Left, Right}}.
ref_build([], [Root]) ->
    Root;
ref_build(L, Q) ->
    {A, L1, Q1} = ref_take(L, Q),
    {B, L2, Q2} = ref_take(L1, Q1),
    ref_build(L2, Q2 ++ [{element(1, A) + element(1, B), {A, B}}]).

ref_take([{LW, _} = Leaf | LT] = L, [{QW, _} = Node | QT] = Q) ->
    if
        QW < LW -> {Node, L, QT};
        true -> {Leaf, LT, Q}
    end;
ref_take([Leaf | LT], []) ->
    {Leaf, LT, []};
ref_take([], [Node | QT]) ->
    {Node, [], QT}.

ref_depths({_, {A, B}}, D) -> ref_depths(A, D + 1) ++ ref_depths(B, D + 1);
ref_depths({_, S}, D) -> [{S, D}].

ref_kraft(Counts, Total, MaxBits) when Total =< 1 bsl MaxBits ->
    Counts;
ref_kraft(Counts, Total, MaxBits) ->
    C1 = setelement(MaxBits, Counts, element(MaxBits, Counts) - 1),
    I = lists:last([J || J <- lists:seq(1, MaxBits - 1), element(J, C1) > 0]),
    C2 = setelement(I + 1, setelement(I, C1, element(I, C1) - 1), element(I + 1, C1) + 2),
    ref_kraft(C2, Total - 1, MaxBits).

ref_assign([], _) -> [];
ref_assign(Syms, [{_, 0} | Counts]) -> ref_assign(Syms, Counts);
ref_assign([{_, S} | T], [{L, K} | Counts]) -> [{S, L} | ref_assign(T, [{L, K - 1} | Counts])].

shuffle(L) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- L])].

huff_lengths(Freqs, MaxBits) ->
    N = length(Freqs),
    {Lens, Counts} = hex_deflate:huff_lengths(Freqs, N, MaxBits),
    {list_to_tuple([atomics:get(Lens, I) || I <- lists:seq(1, N)]), Counts}.

assert_round_trip(Bin, Opts) ->
    ?assertEqual({Opts, Bin}, {Opts, zlib:unzip(hex_deflate:compress(Bin, Opts))}).

kraft(Lengths, MaxBits) ->
    Used = [L || L <- tuple_to_list(Lengths), L > 0],
    {lists:max(Used), lists:sum([1 bsl (MaxBits - L) || L <- Used]) =:= 1 bsl MaxBits}.

workers() ->
    receive
        {worker, Pid} -> [Pid | workers()]
    after 100 -> []
    end.

length_counts(Lengths, MaxBits) ->
    list_to_tuple([
        length([L || L <- tuple_to_list(Lengths), L =:= Bits])
     || Bits <- lists:seq(1, MaxBits)
    ]).

pick(List) ->
    lists:nth(rand:uniform(length(List)), List).

gen(1, N) ->
    rand:bytes(N);
gen(2, N) ->
    <<<<(rand:uniform(4) - 1)>> || _ <- lists:seq(1, N)>>;
gen(3, N) ->
    binary:copy(<<0>>, N);
gen(4, N) ->
    Chunk = rand:bytes(rand:uniform(300)),
    binary:part(binary:copy(Chunk, N div byte_size(Chunk) + 1), 0, N);
gen(5, N) ->
    %% byte K with probability 2^-K
    <<<<(skew(rand:uniform(1 bsl 30), 0))>> || _ <- lists:seq(1, N)>>;
gen(6, N) ->
    binary:part(iolist_to_binary(runs(N)), 0, N);
gen(7, N) ->
    Words = [rand:bytes(rand:uniform(8)) || _ <- lists:seq(1, 50)],
    binary:part(iolist_to_binary([[pick(Words), $\s] || _ <- lists:seq(1, N div 2 + 1)]), 0, N).

skew(R, K) when R band 1 =:= 1; K >= 29 -> K;
skew(R, K) -> skew(R bsr 1, K + 1).

runs(N) when N =< 0 ->
    [];
runs(N) ->
    Length = rand:uniform(600),
    [binary:copy(<<(rand:uniform(256) - 1)>>, Length) | runs(N - Length)].

fib(N) ->
    fib(N, 1, 1, []).

fib(0, _, _, Acc) -> lists:reverse(Acc);
fib(N, A, B, Acc) -> fib(N - 1, B, A + B, [A | Acc]).

%% N bytes from a 32-bit xorshift generator, so that the golden inputs don't
%% depend on the rand module.
golden_bytes(N) ->
    golden_bytes(N, 1, <<>>).

golden_bytes(0, _X, Acc) ->
    Acc;
golden_bytes(N, X0, Acc) ->
    X = xorshift(X0),
    golden_bytes(N - 1, X, <<Acc/binary, (X bsr 24)>>).

%% N bytes of words picked with xorshift, separated by spaces and newlines.
golden_text(N) ->
    Words = list_to_tuple(
        binary:split(
            <<
                "the of and to in is it that for on with as was at by an be this from or "
                "deflate encoder window chunk match literal length distance block huffman "
                "code tree hash chain lazy"
            >>,
            <<" ">>,
            [global]
        )
    ),
    golden_text(N, 7, Words, 0, []).

golden_text(N, _X, _Words, Len, Acc) when Len >= N ->
    binary:part(iolist_to_binary(lists:reverse(Acc)), 0, N);
golden_text(N, X0, Words, Len, Acc) ->
    X = xorshift(X0),
    Word = element(X rem tuple_size(Words) + 1, Words),
    Sep =
        case (X bsr 8) rem 12 of
            0 -> <<"\n">>;
            _ -> <<" ">>
        end,
    golden_text(N, X, Words, Len + byte_size(Word) + 1, [Sep, Word | Acc]).

xorshift(X0) ->
    X1 = X0 bxor ((X0 bsl 13) band 16#FFFFFFFF),
    X2 = X1 bxor (X1 bsr 17),
    X2 bxor ((X2 bsl 5) band 16#FFFFFFFF).
