-module(hex_tarball).
-export([create/2, unpack/2, format_checksum/1]).
-define(VERSION, <<"3">>).
-define(TARBALL_MAX_SIZE, 8 * 1024 * 1024).
-include_lib("kernel/include/file.hrl").

-type checksum() :: binary().
-type contents() :: #{filename() => binary()}.
-type filename() :: string().
-type files() :: [filename()] | contents().
-type metadata() :: map().
-type tarball() :: binary().

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Creates a package tarball.
%%
%% Examples:
%%
%% ```
%%     Metadata = #{<<"app">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
%%     Files = [{"src/foo.erl", <<"-module(foo).">>}],
%%     {ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Files).
%%     Tarball.
%%     %%=> <<86,69,...>>
%%     Checksum.
%%     %%=> <<40,32,...>>
%% '''
%% @end
-spec create(metadata(), files()) -> {ok, {tarball(), checksum()}}.
create(Metadata, Files) ->
    MetadataBinary = encode_metadata(Metadata),
    ContentsBinary = create_tarball(Files, [compressed]),
    Checksum = checksum(?VERSION, MetadataBinary, ContentsBinary),
    ChecksumBase16 = encode_base16(Checksum),

    OuterFiles = [
      {"VERSION", ?VERSION},
      {"CHECKSUM", ChecksumBase16},
      {"metadata.config", MetadataBinary},
      {"contents.tar.gz", ContentsBinary}
    ],

    Tarball = create_tarball(OuterFiles, []),

    case byte_size(Tarball) > ?TARBALL_MAX_SIZE of
        true ->
            {error, {tarball, too_big}};

        false ->
            {ok, {Tarball, Checksum}}
    end.

%% @doc
%% Unpacks a package tarball.
%%
%% Examples:
%%
%% ```
%%     hex_tarball:unpack(Tarball, memory).
%%     %%=> {ok,#{checksum => <<...>>,
%%     %%=>       contents => [{"src/foo.erl",<<"-module(foo).">>}],
%%     %%=>       metadata => #{<<"app">> => <<"foo">>, ...}}}
%%
%%     hex_tarball:unpack(Tarball, "path/to/unpack").
%%     %%=> {ok,#{checksum => <<...>>,
%%     %%=>       metadata => #{<<"app">> => <<"foo">>, ...}}}
%% '''
-spec unpack(tarball(), memory) ->
                {ok, #{checksum => checksum(), metadata => metadata(), contents => contents()}} |
                {error, term()};
            (tarball(), filename()) ->
                {ok, #{checksum => checksum(), metadata => metadata()}} |
                {error, term()}.
unpack(Tarball, _) when byte_size(Tarball) > ?TARBALL_MAX_SIZE ->
    {error, {tarball, too_big}};

unpack(Tarball, Output) ->
    case hex_erl_tar:extract({binary, Tarball}, [memory]) of
        {ok, []} ->
            {error, {tarball, empty}};

        {ok, FileList} ->
            do_unpack(maps:from_list(FileList), Output);

        {error, Reason} ->
            {error, {tarball, Reason}}
    end.

%% @doc
%% Returns base16-encoded representation of checksum.
-spec format_checksum(checksum()) -> string().
format_checksum(Checksum) ->
    encode_base16(Checksum).

%%====================================================================
%% Internal functions
%%====================================================================

checksum(Version, MetadataBinary, ContentsBinary) ->
    Blob = <<Version/binary, MetadataBinary/binary, ContentsBinary/binary>>,
    crypto:hash(sha256, Blob).

encode_metadata(Meta) ->
    Data = lists:map(fun(MetaPair) ->
        String = io_lib_pretty:print(binarify(MetaPair), [{encoding, utf8}]),
        unicode:characters_to_binary([String, ".\n"])
      end, maps:to_list(Meta)),
    iolist_to_binary(Data).

do_unpack(Files, Output) ->
    ?VERSION = maps:get("VERSION", Files),
    Checksum = decode_base16(maps:get("CHECKSUM", Files)),
    ContentsBinary = maps:get("contents.tar.gz", Files),
    Metadata = decode_metadata(maps:get("metadata.config", Files)),
    case unpack_tarball(ContentsBinary, Output) of
        ok ->
            {ok, #{checksum => Checksum, metadata => Metadata}};

        {ok, Contents} ->
            {ok, #{checksum => Checksum, metadata => Metadata, contents => Contents}};

        {error, Reason} ->
            {error, {inner_tarball, Reason}}
    end.

decode_metadata(MetadataBinary) ->
    String = binary_to_list(MetadataBinary),
    case safe_erl_term:string(String) of
        {ok, Tokens, _Line} ->
            try
                maps:from_list(safe_erl_term:terms(Tokens))
            catch
                error:function_clause ->
                    {error, {metadata, invalid_terms}};

                error:badarg ->
                    {error, {metadata, not_key_value}}
            end;

        {error, {_Line, safe_erl_term, Reason}, _Line2} ->
            {error, {metadata, Reason}}
    end.

%%====================================================================
%% Tar Helpers
%%====================================================================

unpack_tarball(ContentsBinary, memory) ->
    hex_erl_tar:extract({binary, ContentsBinary}, [memory, compressed]);
unpack_tarball(ContentsBinary, Output) ->
    hex_erl_tar:extract({binary, ContentsBinary}, [{cwd, Output}, compressed]).

create_tarball(Files, _Options) ->
    create_memory_tarball(Files).

create_memory_tarball(Files) ->
    {ok, Fd} = file:open([], [ram, read, write, binary]),
    {ok, Tar} = hex_erl_tar:init(Fd, write, fun file_op/2),

    try
        try
            add_files(Tar, Files)
        after
            ok = hex_erl_tar:close(Tar)
        end,

        {ok, Size} = file:position(Fd, cur),
        {ok, Binary} = file:pread(Fd, 0, Size),
        Binary
    after
        ok = file:close(Fd)
    end.

file_op(write, {Fd, Data}) -> file:write(Fd, Data);
file_op(position, {Fd, Pos}) -> file:position(Fd, Pos);
file_op(read2, {Fd, Size}) -> file:read(Fd, Size);
file_op(close, _Fd) -> ok.

add_files(Tar, Files) when is_map(Files) ->
    maps:map(fun(Filename, Binary) -> add_file(Tar, {Filename, Binary}) end, Files);
add_files(Tar, Files) when is_list(Files) ->
    lists:map(fun(File) -> add_file(Tar, File) end, Files).

add_file(Tar, {Filename, Contents}) when is_list(Filename) and is_binary(Contents) ->
    ok = hex_erl_tar:add(Tar, Contents, Filename, []);
add_file(Tar, Filename) when is_list(Filename) ->
    ok = hex_erl_tar:add(Tar, Filename, []).

%%====================================================================
%% Helpers
%%====================================================================

binarify(Binary) when is_binary(Binary) -> Binary;
binarify(Number) when is_number(Number) -> Number;
binarify(Atom) when Atom == nil orelse is_boolean(Atom) -> Atom;
binarify(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
binarify(List) when is_list(List) ->
    [binarify(E) || E <- List];
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Map) when is_map(Map) ->
     List = maps:to_list(Map),
     lists:map(fun({K, V}) -> binarify({K, V}) end, List).

encode_base16(Binary) ->
    <<X:256/big-unsigned-integer>> = Binary,
    String = string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))),
    list_to_binary(String).

%% Based on https://github.com/goj/base16/blob/master/src/base16.erl
%% (C) 2012, Erlang Solutions Ltd.

decode_base16(Base16) ->
    << <<(unhex(H) bsl 4 + unhex(L))>> || <<H,L>> <= Base16 >>.

unhex(D) when $0 =< D andalso D =< $9 ->
    D - $0;
unhex(D) when $a =< D andalso D =< $f ->
    10 + D - $a;
unhex(D) when $A =< D andalso D =< $F ->
    10 + D - $A.
