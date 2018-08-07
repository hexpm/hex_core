-module(hex_api).
-export([
    get_package/2,
    get_release/3,
    retire_release/4,
    unretire_release/3,
    me/1,
    create_user/4,
    get_user/2,
    reset_password/2,
    get_keys/1,
    get_key/2,
    add_key/3,
    delete_key/2,
    delete_all_keys/1,
    search/3,
    add_owner/3,
    delete_owner/3,
    get_owner/3,
    get_owners/2
]).
-define(CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

%% @doc
%% Gets package.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_package(<<"package">>, hex_erl:default_options()).
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
get_package(Name, Options) when is_binary(Name) and is_map(Options) ->
    get(<<"/packages/", Name/binary>>, Options).

%% @doc
%% Gets package release.
%%
%% Examples:
%%
%% ```
%%     hex_api:get_release(<<"package">>, <<"1.0.0">>, hex_erl:default_options()).
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
get_release(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    get(<<"/packages/", Name/binary, "/releases/", Version/binary>>, Options).

retire_release(Name, Version, Params, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    post(<<"/packages/", Name/binary, "/releases/", Version/binary, "/retirement">>, Params, Options).

unretire_release(Name, Version, Options) when is_binary(Name) and is_binary(Version) and is_map(Options) ->
    delete(<<"/packages/", Name/binary, "/releases/", Version/binary, "/retirement">>, Options).

me(Options) when is_map(Options) ->
    get(<<"/users/me">>, Options).

create_user(Username, Password, Email, Options) ->
    Params = #{
      <<"username">> => Username,
      <<"password">> => Password,
      <<"email">> => Email
    },
    post(<<"/users">>, Params, Options).

reset_password(Username, Options) when is_binary(Username) and is_map(Options) ->
    post(<<"/users/", Username/binary, "/reset">>, #{}, Options).

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
    get(<<"/users/", Username/binary>>, Options).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%%     hex_api:search(<<"package">>, [], hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"name">> => <<"package1">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
search(Query, SearchParams, Options) when is_binary(Query) and is_list(SearchParams) and is_map(Options) ->
    QueryString = encode_query_string([{search, Query} | SearchParams]),
    get(<<"/packages?", QueryString/binary>>, Options).

%% owners

%% Examples:
%%
%% ```
%%     hex_api:get_owners(<<"package">>, hex_erl:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"username">> => <<"alice">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
get_owners(PackageName, Options) when is_binary(PackageName) and is_map(Options) ->
    get(<<"/packages/", PackageName/binary, "/owners">>, Options).

get_owner(PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    get(<<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, Options).

add_owner(PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    put(<<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, #{}, Options).

delete_owner(PackageName, UsernameOrEmail, Options) when is_binary(PackageName) and is_map(Options) ->
    delete(<<"/packages/", PackageName/binary, "/owners/", UsernameOrEmail/binary>>, Options).

%% keys

get_keys(Options) when is_map(Options) ->
    get(<<"/keys">>, Options).

get_key(Name, Options) when is_map(Options) ->
    get(<<"/keys/", Name/binary>>, Options).

add_key(Name, Permissions, Options) when is_map(Options) ->
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    post(<<"/keys">>, Params, Options).

delete_key(Name, Options) when is_map(Options) ->
    delete(<<"/keys/", Name/binary>>, Options).

delete_all_keys(Options) when is_map(Options) ->
    delete(<<"/keys">>, Options).

%%====================================================================
%% Internal functions
%%====================================================================

get(Path, Options) ->
    request(get, Path, nil, Options).

post(Path, Body, Options) ->
    request(post, Path, encode_body(Body), Options).

put(Path, Body, Options) ->
    request(put, Path, encode_body(Body), Options).

delete(Path, Options) ->
    request(delete, Path, nil, Options).

request(Method, Path, Body, Options) when is_binary(Path) and is_map(Options) ->
    DefaultHeaders = make_headers(Options),
    ReqHeaders = maps:put(<<"accept">>, ?CONTENT_TYPE, DefaultHeaders),

    case hex_http:request(Options, Method, build_url(Path, Options), ReqHeaders, Body) of
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

build_url(Path, #{api_uri := URI, organization := Org}) when is_binary(Org) ->
    <<URI/binary, "/repos/", Org/binary, "/", Path/binary>>;
build_url(Path, #{api_uri := URI}) ->
    <<URI/binary, Path/binary>>.

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
