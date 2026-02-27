-module(hex_safe_binary_to_term_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        safe_complex_term_test,
        safe_atoms_test,
        safe_numbers_test,
        safe_binaries_test,
        safe_bitstrings_test,
        safe_lists_test,
        safe_tuples_test,
        safe_maps_test,
        safe_nested_structures_test,
        safe_pids_test,
        safe_references_test,
        safe_improper_list_test,
        unsafe_function_test,
        unsafe_nested_function_test,
        unsafe_function_in_map_key_test,
        unsafe_function_in_map_value_test,
        unsafe_port_test,
        invalid_binary_test,
        rejects_unknown_atoms_test
    ].

%% Safe term tests

%% Test inspired by Plug.Crypto's non_executable_binary_to_term test:
%% https://github.com/elixir-plug/plug_crypto/blob/c326c3c743b18cf5f4b12735d06dd90c72dcd779/test/plug/crypto_test.exs
safe_complex_term_test(_Config) ->
    %% Complex nested structure similar to Plug.Crypto test:
    %% %{1 => {:foo, ["bar", 2.0, %URI{}, [self() | make_ref()], <<0::4>>]}}
    %% Using Elixir struct representation to test map with __struct__ key
    Struct = #{'__struct__' => 'Elixir.URI', authority => <<"example.com">>},
    Value = #{
        1 => {foo, [<<"bar">>, 2.0, Struct, [self() | make_ref()], <<0:4>>]}
    },
    Binary = term_to_binary(Value),
    {ok, Value} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

safe_atoms_test(_Config) ->
    Term = hello,
    Binary = term_to_binary(Term),
    {ok, hello} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

safe_numbers_test(_Config) ->
    Integer = 42,
    Float = 3.14,
    {ok, 42} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Integer)),
    {ok, 3.14} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Float)),
    ok.

safe_binaries_test(_Config) ->
    Binary = <<"hello world">>,
    {ok, <<"hello world">>} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Binary)),
    ok.

safe_bitstrings_test(_Config) ->
    %% Bitstring that is not byte-aligned (4 bits)
    Bitstring = <<0:4>>,
    {ok, <<0:4>>} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Bitstring)),
    ok.

safe_lists_test(_Config) ->
    List = [1, 2, 3, <<"test">>, atom],
    {ok, [1, 2, 3, <<"test">>, atom]} = hex_safe_binary_to_term:safe_binary_to_term(
        term_to_binary(List)
    ),
    ok.

safe_tuples_test(_Config) ->
    Tuple = {ok, <<"data">>, 123},
    {ok, {ok, <<"data">>, 123}} = hex_safe_binary_to_term:safe_binary_to_term(
        term_to_binary(Tuple)
    ),
    ok.

safe_maps_test(_Config) ->
    Map = #{<<"key">> => <<"value">>, atom_key => 123},
    {ok, #{<<"key">> := <<"value">>, atom_key := 123}} =
        hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Map)),
    ok.

safe_nested_structures_test(_Config) ->
    Nested = #{
        <<"list">> => [1, {2, 3}, #{<<"nested">> => true}],
        <<"tuple">> => {ok, [a, b, c]}
    },
    {ok, Result} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Nested)),
    Nested = Result,
    ok.

safe_pids_test(_Config) ->
    Pid = self(),
    {ok, Pid} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Pid)),
    ok.

safe_references_test(_Config) ->
    Ref = make_ref(),
    {ok, Ref} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(Ref)),
    ok.

safe_improper_list_test(_Config) ->
    %% Improper list: [self() | make_ref()] as in Plug.Crypto test
    ImproperList = [self() | make_ref()],
    {ok, ImproperList} = hex_safe_binary_to_term:safe_binary_to_term(term_to_binary(ImproperList)),
    ok.

%% Unsafe term tests

unsafe_function_test(_Config) ->
    Fun = fun() -> ok end,
    Binary = term_to_binary(Fun),
    {error, {unsafe_term, _}} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

unsafe_nested_function_test(_Config) ->
    %% Test inspired by Plug.Crypto: %{1 => {:foo, [fn -> :bar end]}}
    Fun = fun() -> bar end,
    Nested = #{1 => {foo, [Fun]}},
    Binary = term_to_binary(Nested),
    {error, {unsafe_term, _}} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

unsafe_function_in_map_key_test(_Config) ->
    Fun = fun() -> ok end,
    Map = #{Fun => value},
    Binary = term_to_binary(Map),
    {error, {unsafe_term, _}} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

unsafe_function_in_map_value_test(_Config) ->
    Fun = fun() -> ok end,
    Map = #{key => Fun},
    Binary = term_to_binary(Map),
    {error, {unsafe_term, _}} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

unsafe_port_test(_Config) ->
    Port = open_port({spawn, "cat"}, []),
    Binary = term_to_binary(Port),
    port_close(Port),
    {error, {unsafe_term, _}} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.

%% Error handling tests

invalid_binary_test(_Config) ->
    {error, invalid_term} = hex_safe_binary_to_term:safe_binary_to_term(
        <<"not a valid term">>
    ),
    ok.

%% Test inspired by Plug.Crypto: <<131, 100, 0, 7, 103, 114, 105, 102, 102, 105, 110>>
%% This is a binary encoding of an atom that doesn't exist in the atom table.
%% The 'safe' option should reject it to prevent atom table exhaustion attacks.
rejects_unknown_atoms_test(_Config) ->
    %% Binary encoding of atom 'griffin' which shouldn't exist
    Binary = <<131, 100, 0, 7, 103, 114, 105, 102, 102, 105, 110>>,
    {error, invalid_term} = hex_safe_binary_to_term:safe_binary_to_term(Binary),
    ok.
