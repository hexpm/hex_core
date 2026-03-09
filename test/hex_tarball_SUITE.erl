-module(hex_tarball_SUITE).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        disk_test,
        timestamps_and_permissions_test,
        symlinks_test,
        symlinks_parent_dir_test,
        memory_test,
        build_tools_test,
        requirements_test,
        decode_metadata_test,
        unpack_error_handling_test,
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

    Files = [{"empty", EmptyDir}, {"src", SrcDir}, {"src/foo.erl", Foo}],
    Metadata = #{
        <<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>, <<"build_tool">> => <<"rebar3">>
    },
    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Files
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
        {"empty", EmptyDir},
        {"foo.erl", <<"">>},
        {"foo.sh", Foo}
    ],

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Files
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
        {"dir/foo.sh", FooSh},
        {"dir/bar.sh", BarSh}
    ],

    {ok, #{tarball := Tarball, inner_checksum := InnerChecksum, outer_checksum := OuterChecksum}} = hex_tarball:create(
        Metadata, Files
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
        {"foo.sh", FooSh},
        {"dir/link.sh", LinkSh}
    ],

    {ok, #{tarball := Tarball}} = hex_tarball:create(Metadata, Files),
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
        {"foo.sh", FooSh2},
        {"a/b/link.sh", LinkSh2}
    ],

    {ok, #{tarball := Tarball2}} = hex_tarball:create(Metadata, Files2),
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

    UnsafeFiles = [{"dir/link.sh", UnsafeLink}],
    {ok, #{tarball := UnsafeTarball}} = hex_tarball:create(Metadata, UnsafeFiles),
    UnpackDir3 = filename:join(BaseDir, "symlinks_parent_dir3"),
    {error, {inner_tarball, {"../../escape", unsafe_symlink}}} =
        hex_tarball:unpack(UnsafeTarball, UnpackDir3),

    ok.

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

    BigMetadata = binary:copy(<<"{<<\"k\">>,<<\"v\">>}.\n">>, 10000),
    {error, {tarball, {file_too_big, "metadata.config"}}} =
        unpack_files(OuterFiles#{"metadata.config" => BigMetadata}),

    ok.

too_big_metadata_to_create_test(_Config) ->
    BigValue = binary:copy(<<"x">>, 128 * 1024),
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

unpack_files(Files) ->
    FileList = maps:to_list(Files),
    ok = hex_erl_tar:create("test.tar", FileList, [write]),
    {ok, Binary} = file:read_file("test.tar"),
    ok = file:delete("test.tar"),
    hex_tarball:unpack(Binary, memory).
