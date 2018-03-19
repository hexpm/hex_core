-module(hex_repo).
-export([
    default_options/0,
    get_names/0,
    get_names/1,
    get_versions/0,
    get_versions/1,
    get_package/1,
    get_package/2,
    get_tarball/2,
    get_tarball/3
]).

-type options() :: [{client, client()} | {repo, repo()} | {verify, boolean()}].
-type client() :: #{adapter => hex_http:adapter(), user_agent_string => string()}.
-type repo() :: #{uri => string(), public_key => binary()}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Default options used to interact with the repository.
%% @end
-spec default_options() -> options().
default_options() ->
    %% https://hex.pm/docs/public_keys
    PublicKey = <<"-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApqREcFDt5vV21JVe2QNB
Edvzk6w36aNFhVGWN5toNJRjRJ6m4hIuG4KaXtDWVLjnvct6MYMfqhC79HAGwyF+
IqR6Q6a5bbFSsImgBJwz1oadoVKD6ZNetAuCIK84cjMrEFRkELtEIPNHblCzUkkM
3rS9+DPlnfG8hBvGi6tvQIuZmXGCxF/73hU0/MyGhbmEjIKRtG6b0sJYKelRLTPW
XgK7s5pESgiwf2YC/2MGDXjAJfpfCd0RpLdvd4eRiXtVlE9qO9bND94E7PgQ/xqZ
J1i2xWFndWa6nfFnRxZmCStCOZWYYPlaxr+FZceFbpMwzTNs4g3d4tLNUcbKAIH4
0wIDAQAB
-----END PUBLIC KEY-----">>,
    Client = #{adapter => hex_http_httpc, user_agent_string => "(httpc)"},
    Repo = #{uri => "https://repo.hex.pm", public_key => PublicKey},
    [{client, Client}, {repo, Repo}, {verify, true}].

%% @doc
%% Gets names resource from the repository.
%%
%% Same as `hex_repo:get_names(hex_repo:default_options())'.
%%
%% Examples:
%%
%% ```
%%     hex_repo:get_names().
%%     %%=> {ok, #{package => [
%%     %%=>     #{name => <<"package1">>},
%%     %%=>     #{name => <<"package2">>},
%%     %%=>     ...]}}
%% '''
%% @end
-spec get_names() -> {ok, map()} | {error, term()}.
get_names() ->
    get_names(default_options()).

%% @doc
%% Gets names resource from the repository.
%%
%% See `get_names/1' for examples.
%% @end
-spec get_names(options()) -> {ok, map()} | {error, term()}.
get_names(Options) ->
    Decoder = fun hex_registry:decode_names/1,
    get_protobuf("/names", Decoder, Options).

%% @doc
%% Gets versions resource from the repository.
%%
%% Same as `hex_repo:get_versions(hex_repo:default_options())'.
%%
%% Examples:
%%
%% ```
%%     hex_repo:get_versions().
%%     %%=> {ok, #{packages => [
%%     %%=>     #{name => <<"package1">>, retired => [],
%%     %%=>       versions => [<<"1.0.0">>]},
%%     %%=>     #{name => <<"package2">>, retired => [<<"0.5.0>>"],
%%     %%=>       versions => [<<"0.5.0">>, <<"1.0.0">>]},
%%     %%=>     ...]}}
%% '''
%% @end
-spec get_versions() -> {ok, map()} | {error, term()}.
get_versions() ->
    get_versions(default_options()).

%% @doc
%% Gets versions resource from the repository.
%%
%% See `get_versions/1' for examples.
%% @end
-spec get_versions(options()) -> {ok, map()} | {error, term()}.
get_versions(Options) ->
    Decoder = fun hex_registry:decode_versions/1,
    get_protobuf("/versions", Decoder, Options).

%% @doc
%% Gets package resource from the repository.
%%
%% Same as `hex_repo:get_package(Name, hex_repo:default_options())'.
%%
%% Examples:
%%
%% ```
%%     hex_repo:get_package("package1").
%%     %%=> {ok, #{releases => [
%%     %%=>     #{checksum => ..., version => <<"0.5.0">>, dependencies => []},
%%     %%=>     #{checksum => ..., version => <<"1.0.0">>, dependencies => [
%%     %%=>         #{package => <<"package2">>, optional => true, requirement => <<"~> 0.1">>}
%%     %%=>     ]},
%%     %%=>     ...]}}
%% '''
%% @end
-spec get_package(string()) -> {ok, map()} | {error, term()}.
get_package(Name) ->
    get_package(Name, default_options()).

%% @doc
%% Gets package resource from the repository.
%%
%% See `get_package/1' for examples.
%% @end
-spec get_package(string(), options()) -> {ok, map()} | {error, term()}.
get_package(Name, Options) ->
    Decoder = fun hex_registry:decode_package/1,
    get_protobuf("/packages/" ++ Name, Decoder, Options).

%% @doc
%% Gets tarball from the repository.
%%
%% Same as `hex_repo:get_tarball(Name, Version, hex_repo:default_options())'.
%%
%% ```
%%     {ok, Tarball} = hex_repo:get_tarball("package1", "1.0.0"),
%%     {ok, #{metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
%% '''
%% @end
-spec get_tarball(string(), string()) -> {ok, hex_tarball:tarball()} | {error, term()}.
get_tarball(Name, Version) ->
    get_tarball(Name, Version, default_options()).

%% @doc
%% Gets tarball from the repository.
%%
%% See `get_tarball/2' for examples.
%% @end
-spec get_tarball(string(), string(), options()) -> {ok, hex_tarball:tarball()} | {error, term()}.
get_tarball(Name, Version, Options) ->
    Client = proplists:get_value(client, Options),
    Repo = proplists:get_value(repo, Options),
    case get(Client, tarball_uri(Repo, Name, Version)) of
        {ok, {200, _Headers, Tarball}} ->
            {ok, Tarball};

        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get(Client, URI) ->
    Headers = #{},
    hex_http:get(Client, URI, Headers).

get_protobuf(Path, Decoder, Options) ->
    Client = proplists:get_value(client, Options),
    #{uri := URI, public_key := PublicKey} = proplists:get_value(repo, Options),

    case get(Client, URI ++ Path) of
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
