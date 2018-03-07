-module(hex_repo).
-export([
    get_names/2,
    get_names/3,
    get_versions/2,
    get_versions/3,
    get_package/3,
    get_package/4,
    get_tarball/4
]).

-type repo() :: #{uri => string(), public_key => binary()}.

%%====================================================================
%% API functions
%%====================================================================

-spec get_names(hex_http:adapter(), repo()) -> {ok, map()} | {error, term()}.
get_names(Adapter, Repo) ->
    get_names(Adapter, Repo, []).

-spec get_names(hex_http:adapter(), repo(), [term()]) -> {ok, map()} | {error, term()}.
get_names(Adapter, Repo, Options) ->
    Decoder = fun hex_registry:decode_names/1,
    get_protobuf(Adapter, Repo, "/names", Decoder, Options).

-spec get_versions(hex_http:adapter(), repo()) -> {ok, map()} | {error, term()}.
get_versions(Adapter, Repo) ->
    get_versions(Adapter, Repo, []).

-spec get_versions(hex_http:adapter(), repo(), [term()]) -> {ok, map()} | {error, term()}.
get_versions(Adapter, Repo, Options) ->
    Decoder = fun hex_registry:decode_versions/1,
    get_protobuf(Adapter, Repo, "/versions", Decoder, Options).

-spec get_package(hex_http:adapter(), repo(), binary()) -> {ok, map()} | {error, term()}.
get_package(Adapter, Repo, Name) ->
    get_package(Adapter, Repo, Name, []).

-spec get_package(hex_http:adapter(), repo(), binary(), [term()]) -> {ok, map()} | {error, term()}.
get_package(Adapter, Repo, Name, Options) ->
    Decoder = fun hex_registry:decode_package/1,
    get_protobuf(Adapter, Repo, "/packages/" ++ Name, Decoder, Options).

-spec get_tarball(hex_http:adapter(), repo(), binary(), binary()) -> {ok, map()} | {error, term()}.
get_tarball(Adapter, Repo, Name, Version) ->
    case get(Adapter, tarball_uri(Repo, Name, Version)) of
        {ok, {200, _Headers, Tarball}} ->
            {ok, Tarball};

        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get(Adapter, URI) ->
    Headers = #{},
    hex_http:get(Adapter, URI, Headers).

get_protobuf(Adapter, #{uri := URI, public_key := PublicKey}, Path, Decoder, Options) ->
    case get(Adapter, URI ++ Path) of
        {ok, {200, _Headers, Compressed}} ->
            Signed = zlib:gunzip(Compressed),
            decode(Signed, PublicKey, Decoder, Options);

        {ok, {403, _, _}} ->
            {error, not_found};

        {error, Reason} ->
            {error, Reason}
    end.

decode(Signed, PublicKey, Decoder, Options) ->
    Verify = proplists:get_value(verify, Options, true),

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

tarball_uri(#{uri := URI}, Name, Version) ->
    URI ++ "/tarballs/" ++ Name ++ "-" ++ Version ++ ".tar".
