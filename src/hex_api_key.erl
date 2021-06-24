%% @doc
%% HTTP API - Keys.
-module(hex_api_key).
-export([
    list/1,
    get/2,
    add/3,
    delete/2,
    delete_all/1
]).

-export_type([permission/0]).

-type permission() :: api_permission() | repo_permission() | repos_permission().
-ifdef(OTP_19).
-type api_permission() :: #{binary() := binary()}.
-type repo_permission() :: #{binary() := binary()}.
-type repos_permission() :: #{binary() := binary()}.
-else.
-type api_permission() :: #{binary() => binary()}.
-type repo_permission() :: #{binary() => binary()}.
-type repos_permission() :: #{binary() => binary()}.
-endif.

%% @doc
%% Lists the user's or organization's API and repository keys.
%%
%% Examples:
%%
%% ```
%% > hex_api_key:list(hex_core:default_config()).
%% {ok, {200, ..., [#{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"hex_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"hex_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://hex.pm/api/keys/test">>},
%%     }]}}
%% '''
%% @end
-spec list(hex_core:config()) -> hex_api:response().
list(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys"]),
    hex_api:get(Config, Path).

%% @doc
%% Gets an API or repository key by name.
%%
%% Examples:
%%
%% ```
%% > hex_api_key:get(hex_core:default_config(), <<"test">>).
%% {ok, {200, ..., #{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"hex_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"hex_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://hex.pm/api/keys/test">>},
%%     }}}
%% '''
%% @end
-spec get(hex_core:config(), binary()) -> hex_api:response().
get(Config, Name) when is_map(Config) and is_binary(Name) ->
    Path = hex_api:build_organization_path(Config, ["keys", Name]),
    hex_api:get(Config, Path).

%% @doc
%% Adds a new API or repository key.
%%
%% All keys and values within a `permission()' must be binary otherwise an error is raised.
%%
%% Valid domain values: `<<"api">> | <<"repository">> | <<"repositories">>'.
%%
%% Valid resource values: `<<"read">> | <<"write">>'.
%%
%% Examples:
%%
%% ```
%% > hex_api_key:add(hex_core:default_config(), <<"test">>, [...]).
%% {ok, {200, ..., #{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"hex_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"hex_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://hex.pm/api/keys/test">>},
%%     }}}
%% '''
%% @end

-spec add(hex_core:config(), binary(), [permission()]) -> hex_api:response().
add(Config, Name, Permissions) when is_map(Config) and is_binary(Name) and is_list(Permissions) ->
    assert_valid_permissions(Permissions),
    Path = hex_api:build_organization_path(Config, ["keys"]),
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    hex_api:post(Config, Path, Params).

assert_valid_permissions(Permissions) ->
    FlatPerms = lists:flatmap(fun(P) -> maps:to_list(P) end, Permissions),
    lists:foreach(fun({K,V}) -> assert_permission_key_value(K, V) end, FlatPerms).

assert_permission_key_value(K, V) when not is_binary(K) andalso not is_binary(V) ->
     Format = <<"expected permission key and value to be binaries, got ~p => ~p">>,
     Err =  io_lib:format(Format, [K, V]),
     erlang:error({error, erlang:iolist_to_binary(Err)});

assert_permission_key_value(K, V) when not is_binary(K) andalso is_binary(V) ->
     Format = <<"expected permission key to be a binary, got ~p">>,
     Err = io_lib:format(Format, [K]), 
     erlang:error({error, erlang:iolist_to_binary(Err)});

assert_permission_key_value(K, V) when is_binary(K) andalso not is_binary(V) ->
    Format = "expected permission value for key ~s to be a binary, got ~p",
    Err = io_lib:format(Format, [K, V]),
    erlang:error({error, erlang:iolist_to_binary(Err)});

assert_permission_key_value(_K, _V) ->
    ok.

%% @doc
%% Deletes an API or repository key.
%%
%% Examples:
%%
%% ```
%% > hex_api_key:delete(hex_core:default_config(), <<"test">>).
%% {ok, {200, ..., #{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"hex_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"hex_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://hex.pm/api/keys/test">>},
%%     }}}
%% '''
%% @end
-spec delete(hex_core:config(), binary()) -> hex_api:response().
delete(Config, Name) when is_map(Config) and is_binary(Name) ->
    Path = hex_api:build_organization_path(Config, ["keys", Name]),
    hex_api:delete(Config, Path).

%% @doc
%% Deletes all API and repository keys associated with the account.
%%
%% Examples:
%%
%% ```
%% > hex_api_key:delete_all(hex_core:default_config()).
%% {ok, {200, ..., [#{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"hex_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"hex_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://hex.pm/api/keys/test">>},
%%     }]}}
%% '''
%% @end
-spec delete_all(hex_core:config()) -> hex_api:response().
delete_all(Config) when is_map(Config) ->
    Path = hex_api:build_organization_path(Config, ["keys"]),
    hex_api:delete(Config, Path).
