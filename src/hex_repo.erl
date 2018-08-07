-module(hex_repo).
-export([
    get_names/1,
    get_versions/1,
    get_package/2,
    get_tarball/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Gets names resource from the repository.
%%
%% Examples:
%%
%% ```
%%     hex_repo:get_names(hex_core:default_config()).
%%     %%=> {ok, {200, ...,
%%     %%=>     #{packages => [
%%     %%=>         #{name => <<"package1">>},
%%     %%=>         #{name => <<"package2">>},
%%     %%=>     ]}}}
%% '''
%% @end
get_names(Config) when is_map(Config) ->
    Decoder = fun hex_registry:decode_names/1,
    get_protobuf(Config, <<"/names">>, Decoder).

%% @doc
%% Gets versions resource from the repository.
%%
%% Examples:
%%
%% ```
%%     hex_repo:get_versions(Config).
%%     %%=> {ok, {200, ...,
%%     %%=>     #{packages => [
%%     %%=>         #{name => <<"package1">>, retired => [],
%%     %%=>           versions => [<<"1.0.0">>]},
%%     %%=>         #{name => <<"package2">>, retired => [<<"0.5.0>>"],
%%     %%=>           versions => [<<"0.5.0">>, <<"1.0.0">>]},
%%     %%=>     ]}}}
%% '''
%% @end
get_versions(Config) when is_map(Config) ->
    Decoder = fun hex_registry:decode_versions/1,
    get_protobuf(Config, <<"/versions">>, Decoder).

%% @doc
%% Gets package resource from the repository.
%%
%% Examples:
%%
%% ```
%%     hex_repo:get_package(hex_core:default_config(), <<"package1">>).
%%     %%=> {ok, {200, ...,
%%     %%=>     #{releases => [
%%     %%=>         #{checksum => ..., version => <<"0.5.0">>, dependencies => []},
%%     %%=>         #{checksum => ..., version => <<"1.0.0">>, dependencies => [
%%     %%=>             #{package => <<"package2">>, optional => true, requirement => <<"~> 0.1">>}
%%     %%=>         ]},
%%     %%=>     ]}}}
%% '''
%% @end
get_package(Config, Name) when is_binary(Name) and is_map(Config) ->
    Decoder = fun hex_registry:decode_package/1,
    get_protobuf(Config, <<"/packages/", Name/binary>>, Decoder).

%% @doc
%% Gets tarball from the repository.
%%
%% Examples:
%%
%% ```
%%     {ok, {200, _, Tarball}} = hex_repo:get_tarball(<<"package1">>, <<"1.0.0">>, hex_core:default_config()),
%%     {ok, #{metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
%% '''
%% @end
get_tarball(Config, Name, Version) ->
    URI = maps:get(repo_url, Config),
    ReqHeaders = make_headers(Config),

    case get(Config, tarball_url(URI, Name, Version), ReqHeaders) of
        {ok, {200, RespHeaders, Tarball}} ->
            {ok, {200, RespHeaders, Tarball}};

        Other ->
            Other
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get(Config, URI, Headers) ->
    hex_http:request(Config, get, URI, Headers, nil).

get_protobuf(Config, Path, Decoder) ->
    URI = maps:get(repo_url, Config),
    PublicKey = maps:get(repo_public_key, Config),
    ReqHeaders = make_headers(Config),

    case get(Config, <<URI/binary, Path/binary>>, ReqHeaders) of
        {ok, {200, RespHeaders, Compressed}} ->
            Signed = zlib:gunzip(Compressed),
            case decode(Signed, PublicKey, Decoder, Config) of
                {ok, Decoded} ->
                    {ok, {200, RespHeaders, Decoded}};

                {error, _} = Error ->
                    Error
            end;

        Other ->
            Other
    end.

decode(Signed, PublicKey, Decoder, Config) ->
    Verify = maps:get(repo_verify, Config, true),

    case Verify of
        true ->
            case hex_registry:decode_and_verify_signed(Signed, PublicKey) of
                {ok, Payload} ->
                    {ok, Decoder(Payload)};
                Other ->
                    Other
            end;
        false ->
            #{payload := Payload} = hex_registry:decode_signed(Signed),
            {ok, Decoder(Payload)}
    end.

tarball_url(URI, Name, Version) ->
    Filename = tarball_filename(Name, Version),
    <<URI/binary, "/tarballs/", Filename/binary>>.

tarball_filename(Name, Version) ->
    <<Name/binary, "-", Version/binary, ".tar">>.

make_headers(Config) ->
    maps:fold(fun set_header/3, #{}, Config).

set_header(http_etag, ETag, Headers) -> maps:put(<<"if-none-match">>, ETag, Headers);
set_header(repo_key, Token, Headers) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) -> Headers.
