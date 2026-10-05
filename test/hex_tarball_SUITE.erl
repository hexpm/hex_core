-module(hex_tarball_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        disk_test,
        file_order_test,
        docs_file_order_test,
        duplicate_files_test,
        unicode_normalization_test,
        latin1_filename_encoding_test,
        empty_directories_test,
        read_only_cwd_test,
        read_only_output_parent_test,
        output_under_regular_file_test,
        none_output_temporary_directory_test,
        none_output_unusable_tmpdir_test,
        tmp_dir_owner_only_test,
        pax_record_length_test,
        ustar_prefix_test,
        incomplete_utf8_name_test,
        timestamps_and_permissions_test,
        unpack_bypasses_file_server_test,
        normalized_permissions_test,
        executable_option_test,
        symlinks_test,
        symlinks_parent_dir_test,
        symlink_cycle_test,
        non_ascii_names_latin1_test,
        non_ascii_names_utf8_test,
        unsafe_paths_to_create_test,
        unsupported_file_types_to_create_test,
        memory_test,
        build_tools_test,
        requirements_test,
        metadata_key_order_test,
        metadata_encoding_test,
        metadata_long_string_test,
        metadata_line_length_test,
        printable_range_test,
        invalid_metadata_test,
        decode_metadata_test,
        decode_metadata_backslash_test,
        backslash_metadata_test,
        unpack_error_handling_test,
        gzip_test,
        docs_test,
        too_big_to_create_test,
        too_big_to_unpack_test,
        docs_too_big_to_create_test,
        docs_too_big_to_unpack_test,
        none_test,
        file_unpack_memory_test,
        file_unpack_none_test,
        file_unpack_disk_test,
        file_unpack_too_big_test,
        file_unpack_oversized_inner_files_test,
        oversized_outer_files_test,
        too_big_metadata_to_create_test,
        streamed_extract_test,
        file_unpack_docs_memory_test,
        file_unpack_docs_disk_test,
        file_unpack_docs_too_big_test,
        too_big_uncompressed_to_unpack_test,
        docs_too_big_uncompressed_to_unpack_test,
        file_unpack_too_big_uncompressed_test,
        file_unpack_docs_too_big_uncompressed_test
    ].

too_big_to_create_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"maintainers">> => [<<"José">>],
        <<"build_tool">> => <<"rebar3">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    Config = maps:put(tarball_max_size, 5100, hex_core:default_config()),
    {error, {tarball, {too_big_compressed, 5100}}} = hex_tarball:create(Metadata, Contents, Config),
    Config1 = maps:put(tarball_max_uncompressed_size, 100, hex_core:default_config()),
    {error, {tarball, {too_big_uncompressed, 100}}} = hex_tarball:create(
        Metadata, Contents, Config1
    ),
    ok.

too_big_to_unpack_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"maintainers">> => [<<"José">>],
        <<"build_tool">> => <<"rebar3">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Contents),
    Config = maps:put(tarball_max_size, 5100, hex_core:default_config()),
    {error, {tarball, too_big}} = hex_tarball:unpack(Tarball, memory, Config),
    ok.

memory_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"maintainers">> => [<<"José">>],
        <<"build_tool">> => <<"rebar3">>,
        <<"extra">> => [{<<"foo">>, [{<<"bar">>, <<"baz">>}]}]
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Contents
    ),
    Metadata1 = maps:put(<<"extra">>, #{<<"foo">> => #{<<"bar">> => <<"baz">>}}, Metadata),
    {ok, #{
        inner_checksum := InnerChecksum,
        outer_checksum := OuterChecksum,
        contents := Contents,
        metadata := Metadata1
    }} = hex_tarball:unpack(Tarball, memory),
    ok.

disk_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    SrcDir = filename:join(BaseDir, "src"),
    EmptyDir = filename:join(BaseDir, "empty"),
    Foo = filename:join(SrcDir, "foo.erl"),
    UnpackDir = filename:join(BaseDir, "unpack"),

    ok = file:make_dir(SrcDir),
    ok = file:make_dir(EmptyDir),
    ok = file:change_mode(EmptyDir, 8#100755),
    ok = file:write_file(Foo, <<"-module(foo).">>),
    ok = file:change_mode(Foo, 8#100644),
    ok = file:write_file(filename:join(SrcDir, "not_whitelisted.erl"), <<"">>),

    Files = [{"empty", "empty"}, {"src", "src"}, {"src/foo.erl", filename:join("src", "foo.erl")}],
    Metadata = #{
        <<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>, <<"build_tool">> => <<"rebar3">>
    },
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Files, CreateConfig
    ),
    ?assertEqual(
        <<"3DED88F7BAC738294907ACEED89FE63C9914713F988E608F2B17D7AE6EAC8446">>,
        hex_tarball:format_checksum(OuterChecksum)
    ),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum, metadata := Metadata}} = hex_tarball:unpack(
        Tarball, UnpackDir
    ),
    UnpackedFiles = [
        filename:join(UnpackDir, "empty"),
        filename:join(UnpackDir, "hex_metadata.config"),
        filename:join(UnpackDir, "src"),
        filename:join([UnpackDir, "src", "foo.erl"])
    ],
    ?assertMatch(UnpackedFiles, filelib:wildcard(filename:join(UnpackDir, "**/*"))),
    {ok, <<"-module(foo).">>} = file:read_file(filename:join(UnpackDir, "src/foo.erl")),
    {ok,
        <<"{<<\"build_tool\">>,<<\"rebar3\">>}.\n{<<\"name\">>,<<\"foo\">>}.\n{<<\"version\">>,<<\"1.0.0\">>}.\n">>} =
        file:read_file(filename:join(UnpackDir, "hex_metadata.config")).

file_order_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Dir = filename:join(BaseDir, "file_order"),
    ok = file:make_dir(Dir),
    ok = file:write_file(filename:join(Dir, "a.erl"), <<"-module(a).">>),
    ok = file:write_file(filename:join(Dir, "b.erl"), <<"-module(b).">>),

    Files = [
        {"src/b.erl", filename:join("file_order", "b.erl")},
        {"src/a.erl", filename:join("file_order", "a.erl")},
        {"README.md", <<"readme">>}
    ],
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"files">> => [<<"src/b.erl">>, <<"src/a.erl">>, <<"README.md">>]
    },
    ReversedMetadata = maps:put(
        <<"files">>, lists:reverse(maps:get(<<"files">>, Metadata)), Metadata
    ),
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),

    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),
    {ok, #{tarball := Tarball}} = hex_tarball:create(
        ReversedMetadata, lists:reverse(Files), CreateConfig
    ),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    ?assertEqual(
        ["VERSION", "CHECKSUM", "metadata.config", "contents.tar.gz"],
        [Name || {Name, _} <- OuterFiles]
    ),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    ?assertEqual(
        {ok, ["README.md", "src/a.erl", "src/b.erl"]},
        hex_erl_tar:table({binary, ContentsBinary}, [compressed])
    ),
    {ok, #{metadata := UnpackedMetadata}} = hex_tarball:unpack(Tarball, memory),
    ?assertEqual(
        [<<"README.md">>, <<"src/a.erl">>, <<"src/b.erl">>],
        maps:get(<<"files">>, UnpackedMetadata)
    ).

docs_file_order_test(_Config) ->
    Files = [
        {"index.html", <<"index">>},
        {"api/b.html", <<"b">>},
        {"api/a.html", <<"a">>}
    ],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    {ok, Tarball} = hex_tarball:create_docs(lists:reverse(Files)),
    ?assertEqual(
        {ok, ["api/a.html", "api/b.html", "index.html"]},
        hex_erl_tar:table({binary, Tarball}, [compressed])
    ).

duplicate_files_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Error = {error, {tarball, {duplicate_file, "src/foo.erl"}}},
    ?assertEqual(
        Error,
        hex_tarball:create(Metadata, [{"src/foo.erl", <<"a">>}, {"src/foo.erl", <<"b">>}])
    ),
    ?assertEqual(
        Error,
        hex_tarball:create(Metadata, [{"src/foo.erl", <<"a">>}, {"./src/foo.erl", <<"b">>}])
    ),
    ?assertEqual(Error, hex_tarball:create_docs([{"src/foo.erl", <<>>}, {"src/foo.erl", <<>>}])),
    "duplicate file in tarball: src/foo.erl" = lists:flatten(
        hex_tarball:format_error(element(2, Error))
    ).

unicode_normalization_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Dir = filename:join(BaseDir, "unicode_normalization"),
    ok = file:make_dir(Dir),
    %% "café" with a combining acute accent (NFD), raw names are independent
    %% of the native file name encoding
    ok = file:write_file(<<(raw_path(Dir))/binary, "/cafe", 16#CC, 16#81, ".erl">>, <<"nfd">>),
    ok = file:make_symlink(
        <<"cafe", 16#CC, 16#81, ".erl">>, <<(raw_path(Dir))/binary, "/link.erl">>
    ),
    Nfc = "src/caf" ++ [16#E9] ++ ".erl",
    Nfd = "src/cafe" ++ [16#301] ++ ".erl",
    Files = [
        {Nfd, native_path(filename:join(Dir, "cafe" ++ [16#301] ++ ".erl"))},
        {"src/link.erl", native_path(filename:join(Dir, "link.erl"))}
    ],
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"files">> => [unicode:characters_to_binary(Nfd)]
    },
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    ?assertEqual([Nfc, "src/link.erl"], tar_names(zlib:gunzip(ContentsBinary))),
    ?assertEqual(["caf" ++ [16#E9] ++ ".erl"], tar_linknames(zlib:gunzip(ContentsBinary))),
    {ok, #{metadata := #{<<"files">> := [NfcBinary]}}} = hex_tarball:unpack(Tarball, memory),
    ?assertEqual(unicode:characters_to_binary(Nfc), NfcBinary),

    ?assertEqual(
        {error, {tarball, {duplicate_file, Nfc}}},
        hex_tarball:create(Metadata, [{Nfc, <<"nfc">>}, {Nfd, <<"nfd">>}])
    ).

%% With the latin1 native file name encoding (+fnl) names read from the file
%% system are bytes, the tarball must still be the same
latin1_filename_encoding_test(Config) ->
    case code:ensure_loaded(peer) of
        {module, peer} ->
            BaseDir = ?config(priv_dir, Config),
            Dir = filename:join(BaseDir, "latin1_filename_encoding"),
            ok = file:make_dir(Dir),
            RawDir = raw_path(Dir),
            ok = file:write_file(<<RawDir/binary, "/caf", 16#C3, 16#A9, ".erl">>, <<"café">>),
            ok = file:make_symlink(<<"caf", 16#C3, 16#A9, ".erl">>, <<RawDir/binary, "/link.erl">>),
            Name = "caf" ++ [16#E9] ++ ".erl",
            Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
            CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
            Utf8Files = [
                {"src/" ++ Name, filename:join(Dir, Name)},
                {"src/link.erl", filename:join(Dir, "link.erl")}
            ],
            Latin1Files = [
                {
                    "src/" ++ Name,
                    binary_to_list(unicode:characters_to_binary(filename:join(Dir, Name)))
                },
                {"src/link.erl",
                    binary_to_list(unicode:characters_to_binary(filename:join(Dir, "link.erl")))}
            ],
            {ok, #{tarball := Tarball}} =
                peer_call(["+fnu"], hex_tarball, create, [Metadata, Utf8Files, CreateConfig]),
            {ok, #{tarball := Tarball}} =
                peer_call(["+fnl"], hex_tarball, create, [Metadata, Latin1Files, CreateConfig]),

            {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
            {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
            ?assertEqual([Name], tar_linknames(zlib:gunzip(ContentsBinary)));
        _ ->
            {skip, "peer requires OTP 25"}
    end.

empty_directories_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Dir = filename:join(BaseDir, "empty_directories"),
    ok = filelib:ensure_dir(filename:join([Dir, "empty", "x"])),
    ok = filelib:ensure_dir(filename:join([Dir, "listed", "x"])),
    ok = filelib:ensure_dir(filename:join([Dir, "unlisted", "x"])),
    ok = file:write_file(filename:join([Dir, "listed", "a.erl"]), <<"a">>),
    ok = file:write_file(filename:join([Dir, "unlisted", ".DS_Store"]), <<"junk">>),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [
        {Name, filename:join("empty_directories", Name)}
     || Name <- ["empty", "listed", "listed/a.erl", "unlisted"]
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    {ok, Entries} = hex_erl_tar:table({binary, ContentsBinary}, [compressed, verbose]),
    ?assertEqual(
        [{"empty", directory}, {"listed/a.erl", regular}, {"unlisted", directory}],
        [{Name, Type} || {Name, Type, _Size, _Mtime, _Mode, _Uid, _Gid} <- Entries]
    ).

read_only_cwd_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Dir = filename:join(BaseDir, "read_only_cwd"),
    ok = file:make_dir(Dir),
    ok = file:write_file(filename:join(Dir, "foo.erl"), <<"-module(foo).">>),
    ok = file:change_mode(Dir, 8#555),
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd(Dir),

    try
        Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
        CreateConfig = maps:put(tarball_files_root, Dir, hex_core:default_config()),
        {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, ["foo.erl"], CreateConfig),
        {ok, _} = hex_tarball:create_docs([{"index.html", <<>>}]),
        UnpackDir = filename:join([BaseDir, "read_only_cwd_unpack", "foo"]),
        {ok, _} = hex_tarball:unpack(Tarball, UnpackDir),
        ?assertEqual(
            {ok, <<"-module(foo).">>}, file:read_file(filename:join(UnpackDir, "foo.erl"))
        ),
        ?assertEqual({ok, ["foo"]}, file:list_dir(filename:dirname(UnpackDir))),
        ?assertEqual({ok, ["foo.erl"]}, file:list_dir(Dir))
    after
        ok = file:set_cwd(Cwd),
        ok = file:change_mode(Dir, 8#755)
    end.

read_only_output_parent_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Parent = filename:join(BaseDir, "read_only_output_parent"),
    Output = filename:join(Parent, "foo"),
    ok = file:make_dir(Parent),
    ok = file:make_dir(Output),
    ok = file:change_mode(Parent, 8#555),

    try
        Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
        {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [
            {"foo.erl", <<"-module(foo).">>}
        ]),
        {ok, _} = hex_tarball:unpack(Tarball, Output),
        ?assertEqual({ok, ["foo.erl", "hex_metadata.config"]}, sorted_list_dir(Output)),
        ?assertEqual({ok, ["foo"]}, file:list_dir(Parent))
    after
        ok = file:change_mode(Parent, 8#755)
    end.

output_under_regular_file_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    File = filename:join(BaseDir, "output_under_regular_file"),
    ok = file:write_file(File, <<>>),
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"foo.erl", <<"-module(foo).">>}]),
    ?assertEqual(
        {error, {inner_tarball, enotdir}},
        hex_tarball:unpack(Tarball, filename:join(File, "foo"))
    ).

none_output_temporary_directory_test(Config) ->
    %% Unpacking without an output doesn't touch the current directory, even
    %% one with a file named after the output mode, and cleans up after itself.
    BaseDir = ?config(priv_dir, Config),
    Cwd = filename:join(BaseDir, "none_output_cwd"),
    TmpDir = filename:join(BaseDir, "none_output_tmp"),
    ok = file:make_dir(Cwd),
    ok = file:make_dir(TmpDir),
    ok = file:write_file(filename:join(Cwd, "none"), <<>>),
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"foo.erl", <<"-module(foo).">>}]),
    {ok, OldCwd} = file:get_cwd(),
    OldTmpDir = os:getenv("TMPDIR"),
    ok = file:set_cwd(Cwd),
    true = os:putenv("TMPDIR", TmpDir),

    try
        ?assertMatch({ok, #{metadata := Metadata}}, hex_tarball:unpack(Tarball, none)),
        ?assertEqual({ok, ["none"]}, file:list_dir(Cwd)),
        ?assert(filelib:is_regular(filename:join(Cwd, "none"))),
        ?assertEqual({ok, []}, file:list_dir(TmpDir))
    after
        ok = file:set_cwd(OldCwd),
        case OldTmpDir of
            false -> os:unsetenv("TMPDIR");
            _ -> os:putenv("TMPDIR", OldTmpDir)
        end
    end.

none_output_unusable_tmpdir_test(Config) ->
    %% A TMPDIR that isn't a writable directory is skipped for the next
    %% candidate instead of failing the unpack.
    BaseDir = ?config(priv_dir, Config),
    ReadOnlyDir = filename:join(BaseDir, "unusable_tmpdir_read_only"),
    RegularFile = filename:join(BaseDir, "unusable_tmpdir_file"),
    ok = file:make_dir(ReadOnlyDir),
    ok = file:change_mode(ReadOnlyDir, 8#555),
    ok = file:write_file(RegularFile, <<>>),
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"foo.erl", <<"-module(foo).">>}]),
    OldTmpDir = os:getenv("TMPDIR"),

    try
        lists:foreach(
            fun(TmpDir) ->
                true = os:putenv("TMPDIR", TmpDir),
                ?assertMatch({ok, #{metadata := Metadata}}, hex_tarball:unpack(Tarball, none))
            end,
            [ReadOnlyDir, RegularFile]
        ),
        ?assertEqual({ok, []}, file:list_dir(ReadOnlyDir))
    after
        ok = file:change_mode(ReadOnlyDir, 8#755),
        case OldTmpDir of
            false -> os:unsetenv("TMPDIR");
            _ -> os:putenv("TMPDIR", OldTmpDir)
        end
    end.

tmp_dir_owner_only_test(Config) ->
    %% The outer tarball's files, including the package contents, are only
    %% readable by the owner while they are on disk.
    BaseDir = ?config(priv_dir, Config),
    TmpDir = filename:join([BaseDir, "tmp_dir_owner_only", "tmp"]),
    ok = hex_tarball:make_tmp_dir(TmpDir),
    {ok, #file_info{type = directory, mode = Mode}} = file:read_file_info(TmpDir),
    ?assertEqual(8#700, Mode band 8#777).

sorted_list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Names} -> {ok, lists:sort(Names)};
        Error -> Error
    end.

pax_record_length_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Names = ["lib/" ++ lists:duplicate(N, 16#E9) ++ ".ex" || N <- lists:seq(40, 60)],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{Name, <<"x">>} || Name <- Names]),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    ?assertEqual(Names, tar_names(zlib:gunzip(ContentsBinary))),
    {ok, #{contents := Contents}} = hex_tarball:unpack(Tarball, memory),
    ?assertEqual(Names, [Name || {Name, _} <- Contents]).

ustar_prefix_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Names = [
        lists:duplicate(A, $a) ++ "/" ++ lists:duplicate(B, $b) ++ "/c"
     || A <- lists:seq(75, 79), B <- lists:seq(75, 79)
    ],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{Name, <<"x">>} || Name <- Names]),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    ?assertEqual(lists:sort(Names), tar_names(zlib:gunzip(ContentsBinary))),
    {ok, #{contents := Contents}} = hex_tarball:unpack(Tarball, memory),
    ?assertEqual(lists:sort(Names), [Name || {Name, _} <- Contents]).

%% A ustar name that ends in an incomplete UTF-8 sequence is read as its bytes,
%% without the zero padding of the header field
incomplete_utf8_name_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"cafx", <<"x">>}]),
    {ok, OuterFileList} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    OuterFiles = maps:from_list(OuterFileList),
    #{
        "VERSION" := Version,
        "metadata.config" := MetadataBinary,
        "contents.tar.gz" := ContentsBinary
    } = OuterFiles,

    %% 16#E9 starts a three byte UTF-8 sequence, "caf" followed by it is "café"
    %% in ISO Latin-1
    Contents = hex_tarball:gzip(set_first_name(zlib:gunzip(ContentsBinary), <<"caf", 16#E9>>)),
    Checksum = crypto:hash(sha256, [Version, MetadataBinary, Contents]),
    {ok, #{contents := UnpackedContents}} = unpack_files(OuterFiles#{
        "contents.tar.gz" => Contents,
        "CHECKSUM" => hex_tarball:format_checksum(Checksum)
    }),
    ?assertEqual([{"caf" ++ [16#E9], <<"x">>}], UnpackedContents).

timestamps_and_permissions_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Foo = filename:join(BaseDir, "foo.sh"),
    EmptyDir = filename:join(BaseDir, "timestamps_empty"),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

    ok = file:write_file(Foo, <<"">>),
    ok = file:change_mode(Foo, 8#100755),
    ok = file:make_dir(EmptyDir),
    ok = file:change_mode(EmptyDir, 8#100755),
    Files = [
        {"empty", "timestamps_empty"},
        {"foo.erl", <<"">>},
        {"foo.sh", "foo.sh"}
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Files, CreateConfig
    ),

    %% inside tarball
    {ok, Files2} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, Files2),
    {ok, [EmptyDirEntry, FooErlEntry, FooShEntry]} = hex_erl_tar:table({binary, ContentsBinary}, [
        compressed, verbose
    ]),
    Epoch = epoch(),

    {"empty", directory, _, Epoch, 8#40755, 0, 0} = EmptyDirEntry,
    {"foo.erl", regular, _, Epoch, 8#100644, 0, 0} = FooErlEntry,
    {"foo.sh", regular, _, Epoch, 8#100755, 0, 0} = FooShEntry,

    %% unpacked
    UnpackDir = filename:join(BaseDir, "timestamps_and_permissions"),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:unpack(
        Tarball, UnpackDir
    ),

    {ok, FooErlFileInfo} = file:read_file_info(UnpackDir ++ "/foo.erl"),
    {ok, FooShFileInfo} = file:read_file_info(UnpackDir ++ "/foo.sh"),
    8#100644 = FooErlFileInfo#file_info.mode,
    8#100755 = FooShFileInfo#file_info.mode,
    [{{Year, _, _}, _}] = calendar:local_time_to_universal_time_dst(FooErlFileInfo#file_info.mtime),
    {{Year, _, _}, _} = calendar:local_time().

unpack_bypasses_file_server_test(Config) ->
    %% Files and their info are written with raw file operations so concurrent
    %% unpacks are not serialized through the file server. Directories are
    %% still created through it, once per directory.
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [
        {"src/foo.erl", <<"-module(foo).">>},
        {"src/bar.erl", <<"-module(bar).">>},
        {"priv/nested/data.txt", <<"data">>}
    ],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files),
    UnpackDir = filename:join(BaseDir, "unpack_bypasses_file_server"),

    Requests = trace_file_server_requests(fun() ->
        {ok, _} = hex_tarball:unpack(Tarball, UnpackDir)
    end),

    {ok, <<"data">>} = file:read_file(filename:join(UnpackDir, "priv/nested/data.txt")),
    PerFileRequests = [open, write_file, write_file_info, read_link_info],
    ?assertEqual([], [Request || Request <- Requests, lists:member(Request, PerFileRequests)]).

trace_file_server_requests(Fun) ->
    FileServer = whereis(file_server_2),
    Self = self(),
    Tracer = spawn_link(fun() -> collect_file_server_requests(Self, []) end),
    1 = erlang:trace(FileServer, true, ['receive', {tracer, Tracer}]),
    try
        Fun()
    after
        erlang:trace(FileServer, false, ['receive']),
        Tracer ! {done, Self}
    end,
    receive
        {file_server_requests, Requests} -> Requests
    after 5000 ->
        error(file_server_trace_timeout)
    end.

collect_file_server_requests(Caller, Acc) ->
    receive
        {trace, _, 'receive', {'$gen_call', {Caller, _}, Request}} when is_tuple(Request) ->
            collect_file_server_requests(Caller, [element(1, Request) | Acc]);
        {trace, _, 'receive', _} ->
            collect_file_server_requests(Caller, Acc);
        {done, Caller} ->
            Caller ! {file_server_requests, lists:reverse(Acc)}
    end.

normalized_permissions_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Dir = filename:join(BaseDir, "normalized_permissions"),
    EmptyDir = filename:join(Dir, "empty"),
    ok = file:make_dir(Dir),
    ok = file:make_dir(EmptyDir),
    ok = file:change_mode(EmptyDir, 8#775),
    ok = file:write_file(filename:join(Dir, "group.erl"), <<"">>),
    ok = file:change_mode(filename:join(Dir, "group.erl"), 8#664),
    ok = file:write_file(filename:join(Dir, "private.erl"), <<"">>),
    ok = file:change_mode(filename:join(Dir, "private.erl"), 8#600),
    ok = file:write_file(filename:join(Dir, "script.sh"), <<"">>),
    ok = file:change_mode(filename:join(Dir, "script.sh"), 8#775),
    ok = file:write_file(filename:join(Dir, "owner.sh"), <<"">>),
    ok = file:change_mode(filename:join(Dir, "owner.sh"), 8#700),
    ok = file:make_symlink("group.erl", filename:join(Dir, "link.erl")),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [
        {Name, filename:join("normalized_permissions", Name)}
     || Name <- ["empty", "group.erl", "link.erl", "owner.sh", "private.erl", "script.sh"]
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    {ok, Entries} = hex_erl_tar:table({binary, ContentsBinary}, [compressed, verbose]),
    ?assertEqual(
        [
            {"empty", directory, 8#40755},
            {"group.erl", regular, 8#100644},
            {"link.erl", symlink, 8#120777},
            {"owner.sh", regular, 8#100755},
            {"private.erl", regular, 8#100644},
            {"script.sh", regular, 8#100755}
        ],
        [{Name, Type, Mode} || {Name, Type, _Size, _Mtime, Mode, _Uid, _Gid} <- Entries]
    ).

executable_option_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Dir = filename:join(BaseDir, "executable_option"),
    ok = file:make_dir(Dir),
    ok = file:make_dir(filename:join(Dir, "empty")),
    ok = file:write_file(filename:join(Dir, "script.sh"), <<"">>),
    ok = file:change_mode(filename:join(Dir, "script.sh"), 8#755),
    ok = file:write_file(filename:join(Dir, "run.sh"), <<"">>),
    ok = file:change_mode(filename:join(Dir, "run.sh"), 8#644),
    ok = file:make_symlink("run.sh", filename:join(Dir, "link.sh")),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Path = fun(Name) -> filename:join("executable_option", Name) end,
    Files = [
        {"contents.sh", <<"">>, #{executable => true}},
        {"contents.txt", <<"">>, #{executable => false}},
        {"empty", Path("empty"), #{executable => true}},
        {"link.sh", Path("link.sh"), #{executable => false}},
        {"run.sh", Path("run.sh"), #{executable => true}},
        {"script.sh", Path("script.sh"), #{executable => false}},
        {"unchanged.sh", Path("script.sh"), #{}}
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),

    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, ContentsBinary} = lists:keyfind("contents.tar.gz", 1, OuterFiles),
    {ok, Entries} = hex_erl_tar:table({binary, ContentsBinary}, [compressed, verbose]),
    ?assertEqual(
        [
            {"contents.sh", regular, 8#100755},
            {"contents.txt", regular, 8#100644},
            {"empty", directory, 8#40755},
            {"link.sh", symlink, 8#120777},
            {"run.sh", regular, 8#100755},
            {"script.sh", regular, 8#100644},
            {"unchanged.sh", regular, 8#100755}
        ],
        [{Name, Type, Mode} || {Name, Type, _Size, _Mtime, Mode, _Uid, _Gid} <- Entries]
    ),

    InvalidOptions = [#{executable => yes}, #{mode => 8#755}, #{executable => true, mode => 8#755}],
    lists:foreach(
        fun(Options) ->
            {error, Reason} = hex_tarball:create(Metadata, [{"foo.sh", <<"">>, Options}]),
            ?assertEqual({tarball, {invalid_file_options, "foo.sh", Options}}, Reason),
            ?assertMatch(
                "invalid options for file foo.sh: " ++ _,
                lists:flatten(hex_tarball:format_error(Reason))
            )
        end,
        InvalidOptions
    ).

symlinks_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

    Dir = filename:join(BaseDir, "dir"),
    FooSh = filename:join(Dir, "foo.sh"),
    BarSh = filename:join(Dir, "bar.sh"),
    ok = file:make_dir(Dir),
    ok = file:write_file(FooSh, <<"foo">>),
    ok = file:make_symlink("foo.sh", BarSh),

    Files = [
        {"dir/foo.sh", filename:join("dir", "foo.sh")},
        {"dir/bar.sh", filename:join("dir", "bar.sh")}
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Files, CreateConfig
    ),
    UnpackDir = filename:join(BaseDir, "symlinks"),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:unpack(
        Tarball, UnpackDir
    ),
    {ok, _} = file:read_link_info(filename:join([UnpackDir, "dir", "bar.sh"])),
    ok.

%% OTP's erl_tar validates symlink targets relative to the extraction directory (Cwd),
%% but symlink targets are relative to the symlink's parent directory. This causes
%% safe symlinks with ".." components to be incorrectly rejected.
%%
%% For example, a symlink "dir/link -> ../file" resolves to "file" which is inside the
%% extraction directory, but erl_tar rejects it because "../file" relative to Cwd is
%% considered unsafe.
%%
%% TODO: Fix safe_link_name in erl_tar to validate the resolved path and contribute
%% the fix back to OTP.
symlinks_parent_dir_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

    %% dir/link -> ../file is safe (resolves to file within extraction dir)
    Dir = filename:join(BaseDir, "dir2"),
    FooSh = filename:join(BaseDir, "foo2.sh"),
    LinkSh = filename:join(Dir, "link.sh"),
    ok = file:make_dir(Dir),
    ok = file:write_file(FooSh, <<"foo">>),
    ok = file:make_symlink("../foo2.sh", LinkSh),

    Files = [
        {"foo.sh", "foo2.sh"},
        {"dir/link.sh", filename:join("dir2", "link.sh")}
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),

    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),
    {ok, _} = hex_tarball:unpack(Tarball, memory),

    UnpackDir = filename:join(BaseDir, "symlinks_parent_dir"),
    {ok, _} = hex_tarball:unpack(Tarball, UnpackDir),
    {ok, #file_info{type = symlink}} =
        file:read_link_info(filename:join([UnpackDir, "dir", "link.sh"])),
    {ok, "../foo2.sh"} = file:read_link(filename:join([UnpackDir, "dir", "link.sh"])),

    %% a/b/link -> ../../file is safe (resolves to file within extraction dir)
    ABDir = filename:join([BaseDir, "a2", "b2"]),
    FooSh2 = filename:join(BaseDir, "foo3.sh"),
    LinkSh2 = filename:join(ABDir, "link.sh"),
    ok = filelib:ensure_dir(LinkSh2),
    ok = file:write_file(FooSh2, <<"foo">>),
    ok = file:make_symlink("../../foo3.sh", LinkSh2),

    Files2 = [
        {"foo.sh", "foo3.sh"},
        {"a/b/link.sh", filename:join(["a2", "b2", "link.sh"])}
    ],

    {ok, #{tarball := Tarball2}} = hex_tarball:create(Metadata, Files2, CreateConfig),
    UnpackDir2 = filename:join(BaseDir, "symlinks_parent_dir2"),
    {ok, _} = hex_tarball:unpack(Tarball2, UnpackDir2),
    {ok, #file_info{type = symlink}} =
        file:read_link_info(filename:join([UnpackDir2, "a", "b", "link.sh"])),
    {ok, "../../foo3.sh"} = file:read_link(filename:join([UnpackDir2, "a", "b", "link.sh"])),

    %% dir/link -> ../../escape is unsafe (escapes extraction dir)
    UnsafeDir = filename:join(BaseDir, "unsafe_dir"),
    UnsafeLink = filename:join(UnsafeDir, "link.sh"),
    ok = file:make_dir(UnsafeDir),
    ok = file:make_symlink("../../escape", UnsafeLink),

    UnsafeFiles = [{"dir/link.sh", filename:join("unsafe_dir", "link.sh")}],
    {error, {tarball, {unsafe_symlink, "dir/link.sh", "../../escape"}}} =
        hex_tarball:create(Metadata, UnsafeFiles, CreateConfig),

    ok.

%% dir/loop -> .. resolves inside the extraction dir but forms a cycle, so the
%% post-unpack mtime pass must not traverse symlinked directories.
symlink_cycle_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{<<"name">> => <<"cycle">>, <<"version">> => <<"1.0.0">>},

    Dir = filename:join(BaseDir, "cycle_dir"),
    ok = file:make_dir(Dir),
    ok = file:write_file(filename:join(Dir, "foo.sh"), <<"foo">>),
    ok = file:make_symlink("..", filename:join(Dir, "loop")),

    Files = [
        {"dir/foo.sh", filename:join("cycle_dir", "foo.sh")},
        {"dir/loop", filename:join("cycle_dir", "loop")}
    ],
    CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),

    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files, CreateConfig),
    UnpackDir = filename:join(BaseDir, "symlink_cycle"),
    {ok, _} = hex_tarball:unpack(Tarball, UnpackDir),

    {ok, #file_info{type = symlink}} =
        file:read_link_info(filename:join([UnpackDir, "dir", "loop"])),
    {ok, FooShInfo} = file:read_file_info(filename:join([UnpackDir, "dir", "foo.sh"])),
    [{{Year, _, _}, _}] = calendar:local_time_to_universal_time_dst(FooShInfo#file_info.mtime),
    {{Year, _, _}, _} = calendar:local_time(),
    ok.

%% With the latin1 native file name encoding (+fnl, or a non-UTF-8 locale on
%% Linux) the VM encodes charlist file names one byte per character, so
%% characters above 255 can't be encoded and 128-255 are written as Latin-1.
%% Unpacking must write the UTF-8 bytes stored in the archive in both modes.
non_ascii_names_latin1_test(Config) ->
    non_ascii_names(Config, "+fnl").

non_ascii_names_utf8_test(Config) ->
    non_ascii_names(Config, "+fnu").

non_ascii_names(Config, EncodingFlag) ->
    case code:which(peer) of
        non_existing -> {skip, "peer requires OTP 25 or later"};
        _ -> do_non_ascii_names(Config, EncodingFlag)
    end.

do_non_ascii_names(Config, EncodingFlag) ->
    BaseDir = ?config(priv_dir, Config),
    SourceDir = filename:join(BaseDir, "non_ascii_source"),
    ok = file:make_dir(SourceDir),
    ok = file:make_dir(filename:join(SourceDir, "empty")),
    ok = file:make_dir(filename:join(SourceDir, "sub")),
    ok = file:make_symlink(<<"日本/語.ex"/utf8>>, filename:join(SourceDir, "link")),
    ok = file:make_symlink("..", filename:join([SourceDir, "sub", "up"])),
    ok = file:make_symlink(<<"a/日/.."/utf8>>, filename:join(SourceDir, "escape")),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [
        {"lib/café.ex", <<"café"/utf8>>},
        {"lib/日本/語.ex", <<"語"/utf8>>},
        {"lib/链接.ex", "link"},
        {"priv/données", "empty"}
    ],
    %% a/日 -> .. resolves to the extraction dir, so b -> a/日/.. escapes it
    %% and a/日/../../x climbs above it
    UnsafeSymlinkFiles = [{"a/日", "sub/up"}, {"b", "escape"}],
    UnsafePathFiles = [{"a/日", "sub/up"}, {"a/日/../../x", <<"x">>}],
    CreateConfig = maps:put(tarball_files_root, SourceDir, hex_core:default_config()),

    %% hex_tarball:create/3 reads symlink targets with the native encoding
    {Tarball, DocsTarball, UnsafeSymlinkTarball, UnsafePathTarball} =
        with_peer("+fnu", fun(Peer) ->
            {ok, #{tarball := T1}} =
                peer:call(Peer, hex_tarball, create, [Metadata, Files, CreateConfig]),
            {ok, T2} = peer:call(Peer, hex_tarball, create_docs, [Files, CreateConfig]),
            {ok, #{tarball := T3}} =
                peer:call(Peer, hex_tarball, create, [Metadata, UnsafeSymlinkFiles, CreateConfig]),
            {ok, #{tarball := T4}} =
                peer:call(Peer, hex_tarball, create, [Metadata, UnsafePathFiles, CreateConfig]),
            {T1, T2, T3, T4}
        end),

    UnpackDir = filename:join(BaseDir, "non_ascii_unpack"),
    DocsDir = filename:join(BaseDir, "non_ascii_docs"),
    UnsafeSymlinkDir = filename:join(BaseDir, "non_ascii_unsafe_symlink"),
    UnsafePathDir = filename:join(BaseDir, "non_ascii_unsafe_path"),
    with_peer(EncodingFlag, fun(Peer) ->
        {ok, _} = peer:call(Peer, hex_tarball, unpack, [Tarball, UnpackDir]),
        ok = peer:call(Peer, hex_tarball, unpack_docs, [DocsTarball, DocsDir]),
        {error, {inner_tarball, {"a/日/..", unsafe_symlink}}} =
            peer:call(Peer, hex_tarball, unpack, [UnsafeSymlinkTarball, UnsafeSymlinkDir]),
        {error, {inner_tarball, {"a/日/../../x", unsafe_path}}} =
            peer:call(Peer, hex_tarball, unpack, [UnsafePathTarball, UnsafePathDir])
    end),

    Expected = [
        {<<"lib">>, directory},
        {<<"lib/café.ex"/utf8>>, regular},
        {<<"lib/日本"/utf8>>, directory},
        {<<"lib/日本/語.ex"/utf8>>, regular},
        {<<"lib/链接.ex"/utf8>>, symlink},
        {<<"priv">>, directory},
        {<<"priv/données"/utf8>>, directory}
    ],
    Unpacked = raw_name(UnpackDir),
    ?assertEqual(
        lists:sort([{<<"hex_metadata.config">>, regular} | Expected]), list_raw_tree(Unpacked)
    ),
    ?assertEqual(lists:sort(Expected), list_raw_tree(raw_name(DocsDir))),

    Link = <<Unpacked/binary, "/lib/链接.ex"/utf8>>,
    {ok, LinkTarget} = file:read_link_all(Link),
    ?assertEqual(<<"日本/語.ex"/utf8>>, raw_name(LinkTarget)),
    ?assertEqual({ok, <<"語"/utf8>>}, file:read_file(Link)),
    ?assertEqual({ok, <<"café"/utf8>>}, file:read_file(<<Unpacked/binary, "/lib/café.ex"/utf8>>)),

    %% The mtime pass replaces the Y2K mtimes stored in the tarball
    lists:foreach(
        fun(Name) ->
            {ok, #file_info{mtime = Mtime}} =
                file:read_file_info(<<Unpacked/binary, "/", Name/binary>>, [{time, posix}]),
            ?assert(Mtime > epoch())
        end,
        [<<"lib/café.ex"/utf8>>, <<"lib/日本/語.ex"/utf8>>, <<"priv/données"/utf8>>]
    ),

    UnsafeEntries = [{<<"a">>, directory}, {<<"a/日"/utf8>>, symlink}],
    ?assertEqual(UnsafeEntries, list_raw_tree(raw_name(UnsafeSymlinkDir))),
    ?assertEqual(UnsafeEntries, list_raw_tree(raw_name(UnsafePathDir))),
    ok.

unsafe_paths_to_create_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

    {error, {tarball, {unsafe_path, "../README.md"}}} =
        hex_tarball:create(Metadata, [{"../README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "/README.md"}}} =
        hex_tarball:create(Metadata, [{"/README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "C:\\README.md"}}} =
        hex_tarball:create(Metadata, [{"C:\\README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "..\\README.md"}}} =
        hex_tarball:create(Metadata, [{"..\\README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "../README.md"}}} =
        hex_tarball:create_docs([{"../README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "/README.md"}}} =
        hex_tarball:create_docs([{"/README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "C:\\README.md"}}} =
        hex_tarball:create_docs([{"C:\\README.md", <<"README">>}]),
    {error, {tarball, {unsafe_path, "..\\README.md"}}} =
        hex_tarball:create_docs([{"..\\README.md", <<"README">>}]),

    UnsafeLink = filename:join(BaseDir, "unsafe_link"),
    ok = file:make_symlink("../../README.md", UnsafeLink),
    BaseConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),
    {error, {tarball, {unsafe_symlink, "README.md", "../../README.md"}}} =
        hex_tarball:create(Metadata, [{"README.md", "unsafe_link"}], BaseConfig),
    {error, {tarball, {unsafe_symlink, "README.md", "../../README.md"}}} =
        hex_tarball:create_docs([{"README.md", "unsafe_link"}], BaseConfig),

    RootDir = filename:join(BaseDir, "source_root"),
    OutsideDir = filename:join(BaseDir, "outside"),
    ok = file:make_dir(RootDir),
    ok = file:make_dir(OutsideDir),
    ok = file:make_dir(filename:join(RootDir, "child")),
    ok = file:write_file(filename:join(RootDir, "README.md"), <<"README">>),
    ok = file:write_file(filename:join(OutsideDir, "secret.txt"), <<"secret">>),
    ok = file:make_symlink("../outside", filename:join(RootDir, "link")),
    CreateConfig = maps:put(tarball_files_root, RootDir, hex_core:default_config()),
    DotDotConfig =
        maps:put(
            tarball_files_root, filename:join([RootDir, "child", ".."]), hex_core:default_config()
        ),
    RootReadme = filename:absname(filename:join(RootDir, "README.md")),
    OutsideSecret = filename:absname(filename:join(OutsideDir, "secret.txt")),
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(RootDir),
        {ok, _} = hex_tarball:create(Metadata, [{"README.md", "README.md"}]),
        {ok, _} = hex_tarball:create_docs([{"README.md", "README.md"}]),
        {ok, _} = hex_tarball:create(Metadata, [{"README.md", RootReadme}]),
        {ok, _} = hex_tarball:create_docs([{"README.md", RootReadme}]),
        {error, {tarball, {unsafe_path, "secret.txt"}}} =
            hex_tarball:create(Metadata, [{"secret.txt", OutsideSecret}]),
        {error, {tarball, {unsafe_path, "secret.txt"}}} =
            hex_tarball:create_docs([{"secret.txt", OutsideSecret}])
    after
        ok = file:set_cwd(Cwd)
    end,
    {ok, _} =
        hex_tarball:create(
            Metadata,
            [{"README.md", RootReadme}],
            CreateConfig
        ),
    {ok, _} =
        hex_tarball:create_docs(
            [{"README.md", RootReadme}],
            CreateConfig
        ),
    {ok, _} =
        hex_tarball:create(
            Metadata,
            [{"README.md", RootReadme}],
            DotDotConfig
        ),
    {ok, _} =
        hex_tarball:create_docs(
            [{"README.md", RootReadme}],
            DotDotConfig
        ),
    {error, {tarball, {unsafe_path, "secret.txt"}}} =
        hex_tarball:create(
            Metadata,
            [{"secret.txt", OutsideSecret}],
            DotDotConfig
        ),
    {error, {tarball, {unsafe_path, "secret.txt"}}} =
        hex_tarball:create_docs(
            [{"secret.txt", OutsideSecret}],
            DotDotConfig
        ),
    {error, {tarball, {unsafe_path, "secret.txt"}}} =
        hex_tarball:create(
            Metadata,
            [{"secret.txt", OutsideSecret}],
            CreateConfig
        ),
    {error, {tarball, {unsafe_path, "secret.txt"}}} =
        hex_tarball:create_docs(
            [{"secret.txt", OutsideSecret}],
            CreateConfig
        ),
    {error, {tarball, {unsafe_path, "missing.txt"}}} =
        hex_tarball:create(
            Metadata,
            [{"missing.txt", "../outside/missing.txt"}],
            CreateConfig
        ),
    {error, {tarball, {unsafe_path, "missing.txt"}}} =
        hex_tarball:create_docs(
            [{"missing.txt", "../outside/missing.txt"}],
            CreateConfig
        ),
    {ok, _} =
        hex_tarball:create(Metadata, [{"README.md", "README.md"}], CreateConfig),
    {ok, _} =
        hex_tarball:create_docs([{"README.md", "README.md"}], CreateConfig),
    ok = file:make_symlink("../outside/secret.txt", filename:join(RootDir, "mismatch_link")),
    {error, {tarball, {unsafe_path, "nested/mismatch_link"}}} =
        hex_tarball:create(
            Metadata,
            [{"nested/mismatch_link", "mismatch_link"}],
            CreateConfig
        ),
    {error, {tarball, {unsafe_path, "nested/mismatch_link"}}} =
        hex_tarball:create_docs(
            [{"nested/mismatch_link", "mismatch_link"}],
            CreateConfig
        ),
    {error, {tarball, {unsafe_path, "link/secret.txt"}}} =
        hex_tarball:create(
            Metadata,
            [{"link/secret.txt", "link/secret.txt"}],
            CreateConfig
        ),
    {error, {tarball, {unsafe_path, "link/secret.txt"}}} =
        hex_tarball:create_docs(
            [{"link/secret.txt", "link/secret.txt"}],
            CreateConfig
        ),

    ok.

unsupported_file_types_to_create_test(Config) ->
    case os:find_executable("mkfifo") of
        false ->
            {skip, "mkfifo not available"};
        Mkfifo ->
            BaseDir = ?config(priv_dir, Config),
            Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
            Fifo = filename:join(BaseDir, "fifo"),
            CreateConfig = maps:put(tarball_files_root, BaseDir, hex_core:default_config()),

            _ = file:delete(Fifo),
            [] = os:cmd(Mkfifo ++ " " ++ shell_quote(Fifo)),
            {error, {tarball, {unsupported_file_type, "fifo", other}}} =
                hex_tarball:create(Metadata, [{"fifo", "fifo"}], CreateConfig),
            {error, {tarball, {unsupported_file_type, "fifo", other}}} =
                hex_tarball:create_docs([{"fifo", "fifo"}], CreateConfig),

            ok
    end.

build_tools_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Contents = [],

    {ok, #{tarball := Tarball1}} = hex_tarball:create(
        maps:put(<<"files">>, [<<"Makefile">>], Metadata), Contents
    ),
    {ok, #{metadata := #{<<"build_tools">> := [<<"make">>]}}} = hex_tarball:unpack(
        Tarball1, memory
    ),

    {ok, #{tarball := Tarball2}} = hex_tarball:create(
        maps:put(<<"build_tools">>, [<<"mix">>], Metadata), Contents
    ),
    {ok, #{metadata := #{<<"build_tools">> := [<<"mix">>]}}} = hex_tarball:unpack(Tarball2, memory),

    {ok, #{tarball := Tarball3}} = hex_tarball:create(Metadata, Contents),
    {ok, #{metadata := Metadata2}} = hex_tarball:unpack(Tarball3, memory),
    false = maps:is_key(<<"build_tools">>, Metadata2),

    ok.

requirements_test(_Config) ->
    ExpectedRequirements = #{
        <<"aaa">> => #{
            <<"app">> => <<"aaa">>,
            <<"optional">> => true,
            <<"requirement">> => <<"~> 1.0">>,
            <<"repository">> => <<"hexpm">>
        },
        <<"bbb">> => #{
            <<"app">> => <<"bbb">>,
            <<"optional">> => true,
            <<"requirement">> => <<"~> 1.0">>,
            <<"repository">> => <<"hexpm">>
        }
    },

    Normal = [
        {<<"aaa">>, [
            {<<"app">>, <<"aaa">>},
            {<<"optional">>, true},
            {<<"requirement">>, <<"~> 1.0">>},
            {<<"repository">>, <<"hexpm">>}
        ]},
        {<<"bbb">>, [
            {<<"app">>, <<"bbb">>},
            {<<"optional">>, true},
            {<<"requirement">>, <<"~> 1.0">>},
            {<<"repository">>, <<"hexpm">>}
        ]}
    ],

    Legacy = [
        [
            {<<"name">>, <<"aaa">>},
            {<<"app">>, <<"aaa">>},
            {<<"optional">>, true},
            {<<"requirement">>, <<"~> 1.0">>},
            {<<"repository">>, <<"hexpm">>}
        ],

        [
            {<<"name">>, <<"bbb">>},
            {<<"app">>, <<"bbb">>},
            {<<"optional">>, true},
            {<<"requirement">>, <<"~> 1.0">>},
            {<<"repository">>, <<"hexpm">>}
        ]
    ],

    ExpectedRequirements = hex_tarball:normalize_requirements(Normal),
    ExpectedRequirements = hex_tarball:normalize_requirements(Legacy),
    ok.

%% Small maps with atom keys iterate in atom creation order, so the atoms are
%% created in reverse alphabetical order at runtime.
metadata_key_order_test(_Config) ->
    Zebra = list_to_atom("hex_tarball_suite_zebra"),
    Apple = list_to_atom("hex_tarball_suite_apple"),
    AtomMetadata = #{
        name => <<"foo">>,
        version => <<"1.0.0">>,
        Zebra => <<"z">>,
        Apple => #{Zebra => <<"z">>, Apple => <<"a">>}
    },
    BinaryMetadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"hex_tarball_suite_zebra">> => <<"z">>,
        <<"hex_tarball_suite_apple">> => #{
            <<"hex_tarball_suite_zebra">> => <<"z">>,
            <<"hex_tarball_suite_apple">> => <<"a">>
        }
    },
    Files = [{"src/foo.erl", <<"-module(foo).">>}],

    {ok, #{tarball := Tarball}} = hex_tarball:create(BinaryMetadata, Files),
    {ok, #{tarball := Tarball}} = hex_tarball:create(AtomMetadata, Files),
    ok.

metadata_encoding_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"ascii">> => <<"say \"hi\"\\\n">>,
        <<"latin1">> => <<"Café"/utf8>>,
        <<"unicode">> => <<"日本"/utf8>>,
        <<"invalid">> => <<255, 97>>,
        <<"empty">> => <<>>,
        <<"integers">> => [0, -1, 123456789012345678901234567890],
        <<"atoms">> => [true, false, undefined, nil],
        <<"charlists">> => ["say \"hi\"\n", [26085, 26412], [1, 2]],
        <<"nested">> => #{<<"b">> => [<<"x">>], a => {<<"k">>, <<"v">>}}
    },
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"src/foo.erl", <<>>}]),
    ?assertEqual(
        unicode:characters_to_binary([
            "{<<\"ascii\">>,<<\"say \\\"hi\\\"\\\\\\n\">>}.\n",
            "{<<\"atoms\">>,[true,false,undefined,<<\"nil\">>]}.\n",
            "{<<\"charlists\">>,[\"say \\\"hi\\\"\\n\",[26085,26412],[1,2]]}.\n",
            "{<<\"empty\">>,<<>>}.\n",
            "{<<\"integers\">>,[0,-1,123456789012345678901234567890]}.\n",
            "{<<\"invalid\">>,<<255,97>>}.\n",
            "{<<\"latin1\">>,<<\"Café\"/utf8>>}.\n",
            "{<<\"name\">>,<<\"foo\">>}.\n",
            "{<<\"nested\">>,[{<<\"a\">>,{<<\"k\">>,<<\"v\">>}},{<<\"b\">>,[<<\"x\">>]}]}.\n",
            "{<<\"unicode\">>,<<230,151,165,230,156,172>>}.\n",
            "{<<\"version\">>,<<\"1.0.0\">>}.\n"
        ]),
        metadata_config(Tarball)
    ),
    {ok, #{metadata := Decoded}} = hex_tarball:unpack(Tarball, memory),
    #{
        <<"ascii">> := <<"say \"hi\"\\\n">>,
        <<"latin1">> := <<"Café"/utf8>>,
        <<"unicode">> := <<"日本"/utf8>>,
        <<"invalid">> := <<255, 97>>,
        <<"empty">> := <<>>,
        <<"integers">> := [0, -1, 123456789012345678901234567890],
        <<"atoms">> := [true, false, undefined, <<"nil">>],
        <<"charlists">> := ["say \"hi\"\n", [26085, 26412], [1, 2]]
    } = Decoded,
    ok.

metadata_long_string_test(_Config) ->
    Text = lists:duplicate(300000, $a),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"extra">> => #{<<"text">> => Text}
    },
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"src/foo.erl", <<>>}]),
    ?assert(byte_size(metadata_config(Tarball)) < 300100),
    {ok, #{metadata := #{<<"extra">> := #{<<"text">> := Text}}}} =
        hex_tarball:unpack(Tarball, memory),
    ok.

metadata_line_length_test(_Config) ->
    Files = [<<"lib/file", (integer_to_binary(N))/binary, ".ex">> || N <- lists:seq(1, 5)],
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"files">> => lists:reverse(Files),
        <<"links">> => #{<<"GitHub">> => <<"https://github.com/hexpm/hex_core">>}
    },
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, [{"src/foo.erl", <<>>}]),
    ?assertEqual(
        <<
            "{<<\"files\">>,\n"
            " [<<\"lib/file1.ex\">>,\n"
            "  <<\"lib/file2.ex\">>,\n"
            "  <<\"lib/file3.ex\">>,\n"
            "  <<\"lib/file4.ex\">>,\n"
            "  <<\"lib/file5.ex\">>]}.\n"
            "{<<\"links\">>,[{<<\"GitHub\">>,<<\"https://github.com/hexpm/hex_core\">>}]}.\n"
            "{<<\"name\">>,<<\"foo\">>}.\n"
            "{<<\"version\">>,<<\"1.0.0\">>}.\n"
        >>,
        metadata_config(Tarball)
    ),
    {ok, #{metadata := #{<<"files">> := Files}}} = hex_tarball:unpack(Tarball, memory),
    ok.

%% io_lib_pretty prints non-Latin-1 text as strings only with +pc unicode
printable_range_test(_Config) ->
    case code:ensure_loaded(peer) of
        {module, peer} ->
            Metadata = #{
                <<"name">> => <<"foo">>,
                <<"version">> => <<"1.0.0">>,
                <<"description">> => <<"日本語のパッケージ"/utf8>>,
                <<"extra">> => #{<<"list">> => [26085, 26412]}
            },
            Files = [{"src/foo.erl", <<>>}],
            {ok, #{tarball := Tarball}} =
                peer_call(["+pc", "latin1"], hex_tarball, create, [Metadata, Files]),
            {ok, #{tarball := Tarball}} =
                peer_call(["+pc", "unicode"], hex_tarball, create, [Metadata, Files]),
            ok;
        _ ->
            {skip, "peer requires OTP 25"}
    end.

invalid_metadata_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    Files = [{"src/foo.erl", <<>>}],
    Errors = [
        {{metadata, {unsupported_term, 1.5}}, Metadata#{<<"extra">> => #{<<"float">> => 1.5}}},
        {{metadata, {unsupported_term, {a, b, c}}}, Metadata#{<<"extra">> => [{a, b, c}]}},
        {{metadata, {duplicate_key, <<"description">>}}, Metadata#{
            description => <<"atom">>, <<"description">> => <<"binary">>
        }},
        {{metadata, {duplicate_key, <<"key">>}}, Metadata#{
            <<"extra">> => #{key => <<"atom">>, <<"key">> => <<"binary">>}
        }},
        {{metadata, {invalid_file, "lib/foo.ex"}}, Metadata#{
            <<"files">> => [<<"src/foo.erl">>, "lib/foo.ex"]
        }}
    ],
    lists:foreach(
        fun({Reason, InvalidMetadata}) ->
            ?assertEqual({error, Reason}, hex_tarball:create(InvalidMetadata, Files)),
            true = is_list(lists:flatten(hex_tarball:format_error(Reason)))
        end,
        Errors
    ).

decode_metadata_test(_Config) ->
    #{<<"foo">> := <<"bar">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bar\">>}.">>),

    #{<<"foo">> := <<"bö/utf8">>} = hex_tarball:do_decode_metadata(
        <<"{<<\"foo\">>, <<\"bö/utf8\">>}.">>
    ),

    %% we should convert invalid latin1 encoded metadata to utf8 so that this becomes:
    %% #{<<"foo">> := <<"bö/utf8">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö\">>}.">>),
    #{<<"foo">> := <<"bö">>} = hex_tarball:do_decode_metadata(<<"{<<\"foo\">>, <<\"bö\">>}.">>),

    {error, {metadata, invalid_terms}} = hex_tarball:do_decode_metadata(<<"ok[">>),

    {error, {metadata, {user, "illegal atom asdf"}}} = hex_tarball:do_decode_metadata(<<"asdf.">>),

    {error, {metadata, not_key_value}} = hex_tarball:do_decode_metadata(<<"ok.">>),

    %% Large payload that forces the chunked decoder across many chunks.
    BigPad = binary:copy(<<"x">>, 200 * 1024),
    BigInput = <<"{<<\"name\">>, <<\"", BigPad/binary, "\">>}.\n">>,
    #{<<"name">> := BigPad} = hex_tarball:do_decode_metadata(BigInput),

    %% UTF-8 multi-byte sequence straddling a chunk boundary (chunks are 4 KB).
    %% Pad so the 3-byte UTF-8 char straddles position 4096; the chunked
    %% decoder must buffer the partial bytes across the boundary and produce
    %% the same parsed value as it would for a non-straddling input.
    BoundaryPre = binary:copy(<<"a">>, 4095),
    BoundaryShort = binary:copy(<<"a">>, 100),
    Jp = unicode:characters_to_binary("日本語"),
    Straddle = <<"{<<\"k\">>, <<\"", BoundaryPre/binary, Jp/binary, "\">>}.\n">>,
    NoStraddle = <<"{<<\"k\">>, <<\"", BoundaryShort/binary, Jp/binary, "\">>}.\n">>,
    #{<<"k">> := StraddleVal} = hex_tarball:do_decode_metadata(Straddle),
    #{<<"k">> := NoStraddleVal} = hex_tarball:do_decode_metadata(NoStraddle),
    %% Tail of both should be identical — chunk boundary must not corrupt the
    %% multi-byte sequence.
    TailLen = byte_size(NoStraddleVal) - 100,
    binary:part(NoStraddleVal, 100, TailLen) =:=
        binary:part(StraddleVal, byte_size(StraddleVal) - TailLen, TailLen) orelse
        ct:fail(boundary_mismatch),

    %% Trailing incomplete UTF-8 byte must terminate (regression: previously
    %% looped forever). The post-dot stray byte is unlexable, so we just
    %% require an error tuple — the point is that the call returns at all.
    Truncated = <<"{<<\"k\">>, <<\"v\">>}.\n", 200>>,
    {error, {metadata, _}} = hex_tarball:do_decode_metadata(Truncated),

    %% Latin1 fallback for embedded invalid UTF-8 bytes mid-payload.
    Latin = <<"{<<\"flag\">>, <<\"caf", 233, "\">>}.\n">>,
    #{<<"flag">> := <<"caf", 233>>} = hex_tarball:do_decode_metadata(Latin),

    %% Multiple terms across many chunks — the rest-chars after a dot must
    %% feed forward correctly.
    Multi = iolist_to_binary([
        [<<"{<<\"k">>, integer_to_binary(N), <<"\">>, ">>, integer_to_binary(N), <<"}.\n">>]
     || N <- lists:seq(1, 5000)
    ]),
    MultiResult = hex_tarball:do_decode_metadata(Multi),
    true = is_map(MultiResult),
    5000 = map_size(MultiResult),
    1 = maps:get(<<"k1">>, MultiResult),
    5000 = maps:get(<<"k5000">>, MultiResult),

    %% Field-selective decoding: only requested keys appear in the result.
    Multi2 = <<"{<<\"name\">>, <<\"foo\">>}.\n{<<\"version\">>, <<\"1.0.0\">>}.\n">>,
    #{<<"name">> := <<"foo">>} = Selected = hex_tarball:do_decode_metadata(Multi2, [<<"name">>]),
    1 = map_size(Selected),
    AllSelected = hex_tarball:do_decode_metadata(Multi2, [<<"name">>, <<"version">>]),
    #{<<"name">> := <<"foo">>, <<"version">> := <<"1.0.0">>} = AllSelected,
    2 = map_size(AllSelected),

    %% all matches the no-arg form.
    AllForm = hex_tarball:do_decode_metadata(Multi2, all),
    AllForm = hex_tarball:do_decode_metadata(Multi2),

    %% A huge unwanted form is streamed past without buffering its tokens. The
    %% files list is big enough that buffering would dominate peak memory; the
    %% decoder must skip it without parsing.
    HugePaths = iolist_to_binary([
        [<<"<<\"path/">>, integer_to_binary(N), <<".ex\">>, ">>]
     || N <- lists:seq(1, 10000)
    ]),
    HugeFiles = <<"{<<\"files\">>, [", HugePaths/binary, "<<\"last.ex\">>]}.\n">>,
    HugeMeta =
        <<"{<<\"name\">>, <<\"foo\">>}.\n", HugeFiles/binary,
            "{<<\"version\">>, <<\"1.0.0\">>}.\n">>,
    Skipped = hex_tarball:do_decode_metadata(HugeMeta, [<<"name">>, <<"version">>]),
    #{<<"name">> := <<"foo">>, <<"version">> := <<"1.0.0">>} = Skipped,
    2 = map_size(Skipped),
    false = maps:is_key(<<"files">>, Skipped),

    %% Requesting a non-existent field on otherwise valid metadata returns an
    %% empty map, not an error.
    #{} = NoMatch = hex_tarball:do_decode_metadata(Multi2, [<<"missing">>]),
    0 = map_size(NoMatch),

    %% Field selection still propagates lex errors and malformed-form errors.
    {error, {metadata, not_key_value}} =
        hex_tarball:do_decode_metadata(<<"ok.">>, [<<"name">>]),
    {error, {metadata, {user, "illegal atom asdf"}}} =
        hex_tarball:do_decode_metadata(<<"asdf.">>, [<<"name">>]),

    %% Field selection over chunk-straddling input still works correctly.
    StraddleSelected = hex_tarball:do_decode_metadata(Straddle, [<<"k">>]),
    #{<<"k">> := StraddleVal} = StraddleSelected,

    %% Empty input still errors.
    {error, {metadata, invalid_terms}} = hex_tarball:do_decode_metadata(<<>>, [<<"x">>]),

    ok.

decode_metadata_backslash_test(_Config) ->
    %% {EscapedContents, Value}: a string ends at the first quote that isn't
    %% escaped, so the form after it is decoded on its own.
    Cases = [
        {"a\\\\", <<"a\\">>},
        {"a\\\\\\\\", <<"a\\\\">>},
        {"\\\\", <<"\\">>},
        {"a\\\\\\\"", <<"a\\\"">>},
        {"a\\134", <<"a\\">>},
        {"a\\\nb", <<"ab">>}
    ],
    lists:foreach(
        fun({Escaped, Value}) ->
            Binary = iolist_to_binary([
                "{<<\"k\">>,<<\"", Escaped, "\">>}.\n{<<\"name\">>,<<\"foo\">>}.\n"
            ]),
            Expected = #{<<"k">> => Value, <<"name">> => <<"foo">>},
            Expected = hex_tarball:do_decode_metadata(Binary),
            Expected = hex_tarball:do_decode_metadata(Binary, [<<"k">>, <<"name">>])
        end,
        Cases
    ),

    #{<<"k">> := 'a\\', <<"name">> := true} =
        hex_tarball:do_decode_metadata(<<"{<<\"k\">>,'a\\\\'}.\n{<<\"name\">>,'true'}.\n">>),

    ok.

backslash_metadata_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"description">> => <<"a\\">>,
        <<"maintainers">> => [<<"\\">>, <<"a\\\\">>, <<"a\\\"">>, <<"é\\"/utf8>>],
        <<"links">> => #{<<"GitHub\\">> => <<"https://github.com/\\">>},
        <<"extra">> => #{<<"chars">> => "a\\"}
    },
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),
    {ok, #{metadata := Metadata}} = hex_tarball:unpack(Tarball, memory),

    Fields = [<<"description">>, <<"maintainers">>],
    Config = maps:put(metadata_fields, Fields, hex_core:default_config()),
    {ok, #{metadata := Selected}} = hex_tarball:unpack(Tarball, none, Config),
    Selected = maps:with(Fields, Metadata),

    %% Released Hex clients read \\" at the end of a string as a backslash
    %% followed by an escaped quote, so the final backslash is written as \134.
    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {"metadata.config", MetadataConfig} = lists:keyfind("metadata.config", 1, OuterFiles),
    {_, _} = binary:match(MetadataConfig, <<"{<<\"description\">>,<<\"a\\134\">>}.\n">>),
    {_, _} = binary:match(MetadataConfig, <<"{<<\"chars\">>,\"a\\134\"}">>),

    ok.

unpack_error_handling_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, [{"rebar.config", <<"">>}]
    ),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:unpack(
        Tarball, memory
    ),
    {ok, OuterFileList} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    OuterFiles = maps:from_list(OuterFileList),

    %% tarball

    {error, {tarball, eof}} = hex_tarball:unpack(<<"badtar">>, memory),

    {error, {tarball, empty}} = unpack_files(#{}),

    {error, {tarball, {missing_files, ["VERSION", "CHECKSUM"]}}} =
        unpack_files(#{"metadata.config" => <<"">>, "contents.tar.gz" => <<"">>}),

    {error, {tarball, {bad_version, <<"1">>}}} =
        unpack_files(OuterFiles#{"VERSION" => <<"1">>}),

    {error, {tarball, invalid_inner_checksum}} =
        unpack_files(OuterFiles#{"CHECKSUM" => <<"bad">>}),

    {error, {tarball, {inner_checksum_mismatch, _, _}}} =
        unpack_files(OuterFiles#{"contents.tar.gz" => <<"">>}),

    %% metadata

    Files1 = OuterFiles#{
        "metadata.config" => <<"ok $">>,
        "CHECKSUM" => <<"1BB37F9A91F9E4A3667A4527930187ACF6B9714C0DE7EADD55DC31BE5CFDD98C">>
    },
    {error, {metadata, {illegal, "$"}}} = unpack_files(Files1),

    %% contents

    Files5 = OuterFiles#{
        "contents.tar.gz" => hex_tarball:gzip(<<"badtar">>),
        "CHECKSUM" => <<"1D87B5FFDA2480FC41F282A722FDAE60661349D47E7E084E93190BC242BB4D9C">>
    },
    {error, {inner_tarball, eof}} = unpack_files(Files5),

    Files6 = OuterFiles#{
        "contents.tar.gz" => <<"badgzip">>,
        "CHECKSUM" => <<"C01D8E226CE736680D2D402E5A32A53D6C0DCEA47A773F77A30EE361416FF5BA">>
    },
    {error, {inner_tarball, eof}} = unpack_files(Files6),

    ok.

docs_too_big_to_create_test(_Config) ->
    Files = [{"index.html", <<"Docs">>}],
    Config = maps:put(docs_tarball_max_size, 100, hex_core:default_config()),
    {error, {tarball, {too_big_compressed, 100}}} = hex_tarball:create_docs(Files, Config),
    Config1 = maps:put(docs_tarball_max_uncompressed_size, 100, hex_core:default_config()),
    {error, {tarball, {too_big_uncompressed, 100}}} = hex_tarball:create_docs(Files, Config1),

    ok.

docs_too_big_to_unpack_test(_Config) ->
    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    Config = maps:put(docs_tarball_max_size, 100, hex_core:default_config()),
    {error, {tarball, too_big}} = hex_tarball:unpack_docs(Tarball, memory, Config),

    ok.

gzip_test(_Config) ->
    Uncompressed = <<"-module(foo).\n-module(foo).\n">>,
    Gzip = hex_tarball:gzip(Uncompressed),
    ?assertEqual(
        <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0, 211, 205, 205, 79, 41, 205, 73, 213, 72, 203, 207, 215,
            212, 227, 66, 229, 1, 0, 204, 17, 177, 26, 28, 0, 0, 0>>,
        Gzip
    ),
    ?assertEqual(Uncompressed, zlib:gunzip(Gzip)).

docs_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    UnpackDir = filename:join(BaseDir, "unpack_docs"),

    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    {ok, Files} = hex_tarball:unpack_docs(Tarball, memory),
    ok = hex_tarball:unpack_docs(Tarball, UnpackDir),
    {ok, <<"Docs">>} = file:read_file(filename:join(UnpackDir, "index.html")),

    ok.

none_test(_Config) ->
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tool">> => <<"rebar3">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:create(Metadata, Contents),

    %% Unpack with none output - should return metadata and checksums but no contents
    {ok,
        #{
            inner_checksum := InnerChecksum,
            outer_checksum := OuterChecksum,
            metadata := Metadata
        } = Result} = hex_tarball:unpack(Tarball, none),
    false = maps:is_key(contents, Result),
    ok.

file_unpack_memory_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tool">> => <<"rebar3">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:create(Metadata, Contents),

    %% Write tarball to file
    TarballPath = filename:join(BaseDir, "test_file_unpack.tar"),
    ok = file:write_file(TarballPath, Tarball),

    %% Unpack from file to memory - should return metadata, checksums, and contents
    {ok, #{
        inner_checksum := InnerChecksum,
        outer_checksum := OuterChecksum,
        metadata := Metadata,
        contents := Contents
    }} = hex_tarball:unpack({file, TarballPath}, memory),
    ok.

file_unpack_none_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tool">> => <<"rebar3">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:create(Metadata, Contents),

    %% Write tarball to file
    TarballPath = filename:join(BaseDir, "test_file_unpack_none.tar"),
    ok = file:write_file(TarballPath, Tarball),

    %% Unpack from file with none output - should return metadata and checksums but no contents
    {ok,
        #{
            inner_checksum := InnerChecksum,
            outer_checksum := OuterChecksum,
            metadata := Metadata
        } = Result} = hex_tarball:unpack({file, TarballPath}, none),
    false = maps:is_key(contents, Result),
    ok.

file_unpack_disk_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"build_tool">> => <<"rebar3">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:create(Metadata, Contents),

    %% Write tarball to file
    TarballPath = filename:join(BaseDir, "test_file_unpack_disk.tar"),
    ok = file:write_file(TarballPath, Tarball),

    %% Unpack from file to disk
    UnpackDir = filename:join(BaseDir, "file_unpack_disk"),
    {ok, #{
        inner_checksum := InnerChecksum,
        outer_checksum := OuterChecksum,
        metadata := Metadata
    }} = hex_tarball:unpack({file, TarballPath}, UnpackDir),
    {ok, <<"-module(foo).">>} = file:read_file(filename:join([UnpackDir, "src", "foo.erl"])),
    ok.

file_unpack_too_big_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Contents),

    TarballPath = filename:join(BaseDir, "test_file_too_big.tar"),
    ok = file:write_file(TarballPath, Tarball),

    SmallConfig = maps:put(tarball_max_size, 100, hex_core:default_config()),
    {error, {tarball, too_big}} = hex_tarball:unpack({file, TarballPath}, memory, SmallConfig),
    ok.

file_unpack_oversized_inner_files_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    %% Create a tarball with oversized VERSION file
    BigVersion = binary:copy(<<"3">>, 64),
    Files = [
        {"VERSION", BigVersion},
        {"CHECKSUM", <<"bad">>},
        {"metadata.config", <<"{<<\"name\">>, <<\"foo\">>}.">>},
        {"contents.tar.gz", <<"">>}
    ],
    TarballPath = filename:join(BaseDir, "oversized_inner.tar"),
    ok = hex_erl_tar:create(TarballPath, maps:to_list(maps:from_list(Files)), [write]),
    {error, {tarball, {file_too_big, "VERSION"}}} =
        hex_tarball:unpack({file, TarballPath}, memory),
    ok.

oversized_outer_files_test(_Config) ->
    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, []),
    {ok, FileList} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    OuterFiles = maps:from_list(FileList),

    BigVersion = binary:copy(<<"3">>, 64),
    {error, {tarball, {file_too_big, "VERSION"}}} =
        unpack_files(OuterFiles#{"VERSION" => BigVersion}),

    BigChecksum = binary:copy(<<"A">>, 256),
    {error, {tarball, {file_too_big, "CHECKSUM"}}} =
        unpack_files(OuterFiles#{"CHECKSUM" => BigChecksum}),

    BigMetadata = binary:copy(<<"{<<\"k\">>,<<\"v\">>}.\n">>, 60000),
    {error, {tarball, {file_too_big, "metadata.config"}}} =
        unpack_files(OuterFiles#{"metadata.config" => BigMetadata}),

    ok.

too_big_metadata_to_create_test(_Config) ->
    BigValue = binary:copy(<<"x">>, 1024 * 1024),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>,
        <<"description">> => BigValue
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {error, {tarball, {file_too_big, "metadata.config"}}} =
        hex_tarball:create(Metadata, Contents),
    ok.

%% Test that extracting to disk streams file entries in chunks
%% instead of loading them fully into memory.
streamed_extract_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},

    %% Create test files of various sizes
    EmptyData = <<>>,
    SmallData = <<"hello">>,

    %% A file exactly equal to the default chunk size (65536 bytes)
    ChunkSize = 65536,
    BoundaryData = crypto:strong_rand_bytes(ChunkSize),

    %% A file larger than the default chunk size
    LargeSize = 200000,
    LargeData = crypto:strong_rand_bytes(LargeSize),

    Contents = [
        {"empty", EmptyData},
        {"small", SmallData},
        {"boundary", BoundaryData},
        {"large", LargeData}
    ],

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:create(Metadata, Contents),

    %% Extract from binary to disk and verify contents
    UnpackDir1 = filename:join(BaseDir, "streamed_extract_binary"),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:unpack(Tarball, UnpackDir1),
    {ok, EmptyData} = file:read_file(filename:join(UnpackDir1, "empty")),
    {ok, SmallData} = file:read_file(filename:join(UnpackDir1, "small")),
    {ok, BoundaryData} = file:read_file(filename:join(UnpackDir1, "boundary")),
    {ok, LargeData} = file:read_file(filename:join(UnpackDir1, "large")),

    %% Extract from file to disk and verify contents
    TarballPath = filename:join(BaseDir, "streamed_extract.tar"),
    ok = file:write_file(TarballPath, Tarball),
    UnpackDir2 = filename:join(BaseDir, "streamed_extract_file"),
    {ok, #{inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} =
        hex_tarball:unpack({file, TarballPath}, UnpackDir2),
    {ok, EmptyData} = file:read_file(filename:join(UnpackDir2, "empty")),
    {ok, SmallData} = file:read_file(filename:join(UnpackDir2, "small")),
    {ok, BoundaryData} = file:read_file(filename:join(UnpackDir2, "boundary")),
    {ok, LargeData} = file:read_file(filename:join(UnpackDir2, "large")),

    %% Verify that memory extraction still works (not affected by streaming)
    {ok, #{contents := MemContents}} = hex_tarball:unpack(Tarball, memory),
    MemMap = maps:from_list(MemContents),
    EmptyData = maps:get("empty", MemMap),
    SmallData = maps:get("small", MemMap),
    BoundaryData = maps:get("boundary", MemMap),
    LargeData = maps:get("large", MemMap),

    ok.

file_unpack_docs_memory_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    TarballPath = filename:join(BaseDir, "docs.tar.gz"),
    ok = file:write_file(TarballPath, Tarball),

    {ok, Files} = hex_tarball:unpack_docs({file, TarballPath}, memory),

    ok.

file_unpack_docs_disk_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    TarballPath = filename:join(BaseDir, "docs_disk.tar.gz"),
    ok = file:write_file(TarballPath, Tarball),

    UnpackDir = filename:join(BaseDir, "unpack_file_docs"),
    ok = hex_tarball:unpack_docs({file, TarballPath}, UnpackDir),
    {ok, <<"Docs">>} = file:read_file(filename:join(UnpackDir, "index.html")),

    ok.

file_unpack_docs_too_big_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    TarballPath = filename:join(BaseDir, "docs_big.tar.gz"),
    ok = file:write_file(TarballPath, Tarball),

    SmallConfig = maps:put(docs_tarball_max_size, 10, hex_core:default_config()),
    {error, {tarball, too_big}} = hex_tarball:unpack_docs({file, TarballPath}, memory, SmallConfig),

    ok.

too_big_uncompressed_to_unpack_test(CtConfig) ->
    BaseDir = ?config(priv_dir, CtConfig),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Contents),

    %% Uncompressed size limit too small - memory
    Config = maps:put(tarball_max_uncompressed_size, 1, hex_core:default_config()),
    {error, {inner_tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack(Tarball, memory, Config),

    %% Uncompressed size limit too small - disk
    UnpackDir = filename:join(BaseDir, "too_big_uncompressed"),
    {error, {inner_tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack(Tarball, UnpackDir, Config),

    %% Uncompressed size limit large enough
    Config2 = maps:put(tarball_max_uncompressed_size, 10 * 1024 * 1024, hex_core:default_config()),
    {ok, _} = hex_tarball:unpack(Tarball, memory, Config2),
    ok.

docs_too_big_uncompressed_to_unpack_test(CtConfig) ->
    BaseDir = ?config(priv_dir, CtConfig),
    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),

    %% Uncompressed size limit too small - memory
    Config = maps:put(docs_tarball_max_uncompressed_size, 1, hex_core:default_config()),
    {error, {tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack_docs(Tarball, memory, Config),

    %% Uncompressed size limit too small - disk
    UnpackDir = filename:join(BaseDir, "docs_too_big_uncompressed"),
    {error, {tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack_docs(Tarball, UnpackDir, Config),

    %% Uncompressed size limit large enough
    Config2 = maps:put(
        docs_tarball_max_uncompressed_size, 10 * 1024 * 1024, hex_core:default_config()
    ),
    {ok, _} = hex_tarball:unpack_docs(Tarball, memory, Config2),
    ok.

file_unpack_too_big_uncompressed_test(Config) ->
    BaseDir = ?config(priv_dir, Config),
    Metadata = #{
        <<"name">> => <<"foo">>,
        <<"version">> => <<"1.0.0">>
    },
    Contents = [{"src/foo.erl", <<"-module(foo).">>}],
    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Contents),

    TarballPath = filename:join(BaseDir, "test_file_too_big_uncompressed.tar"),
    ok = file:write_file(TarballPath, Tarball),

    %% Memory unpack from file
    SmallConfig = maps:put(tarball_max_uncompressed_size, 1, hex_core:default_config()),
    {error, {inner_tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack({file, TarballPath}, memory, SmallConfig),

    %% Disk unpack from file
    UnpackDir = filename:join(BaseDir, "file_unpack_too_big_uncompressed"),
    {error, {inner_tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack({file, TarballPath}, UnpackDir, SmallConfig),
    ok.

file_unpack_docs_too_big_uncompressed_test(Config) ->
    BaseDir = ?config(priv_dir, Config),

    Files = [{"index.html", <<"Docs">>}],
    {ok, Tarball} = hex_tarball:create_docs(Files),
    TarballPath = filename:join(BaseDir, "docs_big_uncompressed.tar.gz"),
    ok = file:write_file(TarballPath, Tarball),

    %% Memory unpack from file
    SmallConfig = maps:put(docs_tarball_max_uncompressed_size, 1, hex_core:default_config()),
    {error, {tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack_docs({file, TarballPath}, memory, SmallConfig),

    %% Disk unpack from file
    UnpackDir = filename:join(BaseDir, "file_unpack_docs_too_big_uncompressed"),
    {error, {tarball, {too_big_uncompressed, 1}}} =
        hex_tarball:unpack_docs({file, TarballPath}, UnpackDir, SmallConfig),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

epoch() ->
    NixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Y2kEpoch = calendar:datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}}),
    Y2kEpoch - NixEpoch.

metadata_config(Tarball) ->
    {ok, OuterFiles} = hex_erl_tar:extract({binary, Tarball}, [memory]),
    {_, MetadataBinary} = lists:keyfind("metadata.config", 1, OuterFiles),
    MetadataBinary.

raw_path(Path) ->
    case file:native_name_encoding() of
        utf8 -> unicode:characters_to_binary(Path);
        latin1 -> list_to_binary(Path)
    end.

native_path(Path) ->
    case file:native_name_encoding() of
        utf8 -> Path;
        latin1 -> binary_to_list(unicode:characters_to_binary(Path))
    end.

peer_call(Args, Module, Function, Arguments) ->
    Peer =
        case peer:start_link(#{connection => standard_io, args => Args}) of
            {ok, Pid} -> Pid;
            {ok, Pid, _Node} -> Pid
        end,
    try
        true = peer:call(Peer, code, add_path, [filename:dirname(code:which(hex_tarball))]),
        peer:call(Peer, Module, Function, Arguments, 30000)
    after
        peer:stop(Peer)
    end.

tar_names(Tar) ->
    [unicode:characters_to_list(Name) || {_Type, Name, _Linkname} <- tar_entries(Tar)].

tar_linknames(Tar) ->
    [unicode:characters_to_list(Linkname) || {$2, _Name, Linkname} <- tar_entries(Tar)].

%% Reads the entries of an uncompressed tar independently of hex_erl_tar,
%% PAX records are parsed by their declared length
tar_entries(Tar) ->
    tar_entries(Tar, #{}, []).

tar_entries(<<Header:512/binary, Rest/binary>>, Pax, Acc) ->
    case Header of
        <<0:4096>> ->
            lists:reverse(Acc);
        _ ->
            Size = tar_octal(binary:part(Header, 124, 12)),
            Padded = (Size + 511) div 512 * 512,
            <<Data:Size/binary, _:(Padded - Size)/binary, Rest2/binary>> = Rest,
            case binary:at(Header, 156) of
                $x ->
                    tar_entries(Rest2, pax_records(Data), Acc);
                Type ->
                    Name = maps:get(<<"path">>, Pax, ustar_name(Header)),
                    Linkname = maps:get(
                        <<"linkpath">>, Pax, tar_string(binary:part(Header, 157, 100))
                    ),
                    tar_entries(Rest2, #{}, [{Type, Name, Linkname} | Acc])
            end
    end.

pax_records(<<>>) ->
    #{};
pax_records(Data) ->
    [LengthBinary, _] = binary:split(Data, <<" ">>),
    Length = binary_to_integer(LengthBinary),
    <<Record:Length/binary, Rest/binary>> = Data,
    <<_:(byte_size(LengthBinary) + 1)/binary, KeyValue/binary>> = Record,
    $\n = binary:last(KeyValue),
    [Key, Value] = binary:split(binary:part(KeyValue, 0, byte_size(KeyValue) - 1), <<"=">>),
    maps:put(Key, Value, pax_records(Rest)).

ustar_name(Header) ->
    Name = tar_string(binary:part(Header, 0, 100)),
    case tar_string(binary:part(Header, 345, 155)) of
        <<>> -> Name;
        Prefix -> <<Prefix/binary, "/", Name/binary>>
    end.

%% Replaces the name in the first header of an uncompressed tar and updates the
%% checksum of the header
set_first_name(
    <<_:100/binary, Fields:48/binary, _Checksum:8/binary, Tail:356/binary, Rest/binary>>, Name
) ->
    NameField = <<Name/binary, 0:((100 - byte_size(Name)) * 8)>>,
    Header = <<NameField/binary, Fields/binary, "        ", Tail/binary>>,
    Checksum = iolist_to_binary(io_lib:format("~6.8.0B", [lists:sum(binary_to_list(Header))])),
    <<NameField/binary, Fields/binary, Checksum/binary, 0, $\s, Tail/binary, Rest/binary>>.

tar_string(Field) ->
    hd(binary:split(Field, <<0>>)).

tar_octal(Field) ->
    list_to_integer(string:trim(binary_to_list(tar_string(Field))), 8).

shell_quote(String) ->
    "'" ++ lists:flatten(string:replace(String, "'", "'\\''", all)) ++ "'".

unpack_files(Files) ->
    FileList = maps:to_list(Files),
    ok = hex_erl_tar:create("test.tar", FileList, [write]),
    {ok, Binary} = file:read_file("test.tar"),
    ok = file:delete("test.tar"),
    hex_tarball:unpack(Binary, memory).

with_peer(EncodingFlag, Fun) ->
    Ebin = filename:dirname(code:which(hex_tarball)),
    {ok, Peer, _Node} = peer:start_link(#{
        connection => standard_io, args => [EncodingFlag, "-pa", Ebin]
    }),
    try
        Fun(Peer)
    after
        peer:stop(Peer)
    end.

%% Returns the bytes the VM passes to the OS for a file name.
raw_name(Name) when is_binary(Name) ->
    Name;
raw_name(Name) ->
    unicode:characters_to_binary(Name, unicode, file:native_name_encoding()).

%% Lists the entries below Dir as {RelativePath, Type} with raw file names.
list_raw_tree(Dir) ->
    lists:sort(list_raw_tree(Dir, <<>>)).

list_raw_tree(Dir, Prefix) ->
    {ok, Names} = file:list_dir_all(Dir),
    lists:flatmap(
        fun(Name) ->
            RawName = raw_name(Name),
            Path = <<Dir/binary, "/", RawName/binary>>,
            Relative = <<Prefix/binary, RawName/binary>>,
            case file:read_link_info(Path) of
                {ok, #file_info{type = directory}} ->
                    [{Relative, directory} | list_raw_tree(Path, <<Relative/binary, "/">>)];
                {ok, #file_info{type = Type}} ->
                    [{Relative, Type}]
            end
        end,
        Names
    ).
