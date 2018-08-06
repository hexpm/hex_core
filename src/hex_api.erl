-module(hex_api).
-export([
    get_package/3,
    get_release/4,
    retire_release/5,
    unretire_release/4,
    me/1,
    create_user/4,
    get_user/2,
    reset_password/2,
    get_keys/1,
    get_key/2,
    add_key/3,
    delete_key/2,
    delete_all_keys/1,
    search/4,
    add_owner/4,
    delete_owner/4,
    get_owner/4,
    get_owners/3
]).
-define(CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

%% @doc
%% Gets package.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_package(nil, <<"package">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., #{
%%     %%=>     <<"name">> => <<"package1">>,
%%     %%=>     <<"meta">> => #{
%%     %%=>         <<"description">> => ...,
%%     %%=>         <<"licenses">> => ...,
%%     %%=>         <<"links">> => ...,
%%     %%=>         <<"maintainers">> => ...
%%     %%=>     },
%%     %%=>     ...,
%%     %%=>     <<"releases">> => [
%%     %%=>         #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%     %%=>         #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%     %%=>         ...
%%     %%=>     ]}}}
%% '''
%% @end
get_package(Repo, Name, Options) when is_binary(Name) and is_map(Options) ->
    get(Repo, <<"/packages/", Name/binary>>, Options).

%% @doc
%% Gets package release.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_release(nil, <<"package">>, <<"1.0.0">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., #{
%%     %%=>     <<"version">> => <<"1.0.0">>,
%%     %%=>     <<"meta">> => #{
%%     %%=>         <<"description">> => ...,
%%     %%=>         <<"licenses">> => ...,
%%     %%=>         <<"links">> => ...,
%%     %%=>         <<"maintainers">> => ...
%%     %%=>     },
%%     %%=>     ...}}}
%% '''
%% @end
get_release(Repo, Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    get(Repo, <<"/packages/", Name/binary, "/releases/", Version/binary>>, Options).

retire_release(Repo, Name, Version, Params, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    post(Repo, <<"/packages/", Name/binary, "/releases/", Version/binary, "/retirement">>, Params, Options).

unretire_release(Repo, Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    delete(Repo, <<"/packages/", Name/binary, "/releases/", Version/binary, "/retirement">>, Options).

me(Options) when is_map(Options) ->
    get(nil, <<"/users/me">>, Options).

create_user(Username, Password, Email, Options) ->
    Params = #{
      <<"username">> => Username,
      <<"password">> => Password,
      <<"email">> => Email
    },
    post(nil, <<"/users">>, Params, Options).

reset_password(Username, Options) when is_binary(Username) and is_map(Options) ->
    post(nil, <<"/users/", Username/binary, "/reset">>, #{}, Options).

%% @doc
%% Gets user.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_user(<<"user">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., #{
%%     %%=>     <<"username">> => <<"user">>,
%%     %%=>     <<"packages">> => [
%%     %%=>         #{
%%     %%=>             <<"name">> => ...,
%%     %%=>             <<"url">> => ...,
%%     %%=>             ...
%%     %%=>         },
%%     %%=>         ...
%%     %%=>     ],
%%     %%=>     ...}}}
%% '''
%% @end
get_user(Username, Options) when is_binary(Username) and is_map(Options) ->
    get(nil, <<"/users/", Username/binary>>, Options).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%%     hex_api:search(nil, <<"package">>, [], hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"name">> => <<"package1">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
search(Repo, Query, SearchParams, Options) when is_binary(Query) and is_list(SearchParams) and is_map(Options) ->
    QueryString = encode_query_string([{search, Query} | SearchParams]),
    get(Repo, <<"/packages?", QueryString/binary>>, Options).

%% owners

%% Examples:
%%
%% ```
%%     hex_api:get_owners(nil, <<"package">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"username">> => <<"alice">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
get_owners(Repo, PackageName, Options) when is_binary(PackageName) and is_map(Options) ->
    get(Repo, <<"/packages/", PackageName/binary, "/owners">>, Options).

get_owner(Repo, PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    get(Repo, <<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, Options).

add_owner(Repo, PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    put(Repo, <<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, #{}, Options).

delete_owner(Repo, PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    delete(Repo, <<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, Options).

%% keys

get_keys(Options) when is_map(Options) ->
    get(nil, <<"/keys">>, Options).

get_key(Name, Options) when is_map(Options) ->
    get(nil, <<"/keys/", Name/binary>>, Options).

add_key(Name, Permissions, Options) when is_map(Options) ->
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    post(nil, <<"/keys">>, Params, Options).

delete_key(Name, Options) when is_map(Options) ->
    delete(nil, <<"/keys/", Name/binary>>, Options).

delete_all_keys(Options) when is_map(Options) ->
    delete(nil, <<"/keys">>, Options).

%%====================================================================
%% Internal functions
%%====================================================================

get(Repo, Path, Options) ->
    request(Repo, get, Path, nil, Options).

post(Repo, Path, Body, Options) ->
    request(Repo, post, Path, encode_body(Body), Options).

put(Repo, Path, Body, Options) ->
    request(Repo, put, Path, encode_body(Body), Options).

delete(Repo, Path, Options) ->
    request(Repo, delete, Path, nil, Options).

request(Repo, Method, Path, Body, Options) when is_binary(Path) and is_map(Options) ->
    DefaultHeaders = make_headers(Options),
    ReqHeaders = maps:put(<<"accept">>, ?CONTENT_TYPE, DefaultHeaders),

    case hex_http:request(Options, Method, build_url(Repo, Path, Options), ReqHeaders, Body) of
        {ok, {Status, RespHeaders, RespBody} = Response} ->
            ContentType = maps:get(<<"content-type">>, RespHeaders, <<"">>),
            case binary:match(ContentType, ?CONTENT_TYPE) of
                {_, _} ->
                    {ok, {Status, RespHeaders, binary_to_term(RespBody)}};

                nomatch ->
                    Response
            end;

        Other ->
            Other
    end.

build_url(nil, Path, #{api_uri := URI}) ->
    <<URI/binary, Path/binary>>;
build_url(Repo, Path, #{api_uri := URI}) ->
    <<"/repos/", Repo/binary, "/", URI/binary, Path/binary>>.

encode_body(Body) ->
    {binary_to_list(?CONTENT_TYPE), term_to_binary(Body)}.

encode_query_string(List) ->
    QueryString =
        join("&",
            lists:map(fun
                ({K, V}) when is_atom(V) ->
                    atom_to_list(K) ++ "=" ++ atom_to_list(V);
                ({K, V}) when is_binary(V) ->
                    atom_to_list(K) ++ "=" ++ binary_to_list(V);
                ({K, V}) when is_integer(V) ->
                    atom_to_list(K) ++ "=" ++ integer_to_list(V)
            end, List)),
    Encoded = http_uri:encode(QueryString),
    list_to_binary(Encoded).

%% https://github.com/erlang/otp/blob/OTP-20.3/lib/stdlib/src/lists.erl#L1449:L1453
join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

%% TODO: copy-pasted from hex_repo
make_headers(Options) ->
    maps:fold(fun set_header/3, #{}, Options).

set_header(api_key, Token, Headers) -> maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) -> Headers.
