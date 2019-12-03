-module(hex_api_auth).
-export([
    test_key/3
]).

-define(ERL_CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

%% @doc
%% Test an auth key against a given domain and resource.
%%
%% Examples:
%%
%% ```
%% 1> Params = [{domain, <<"repository">>}, {resource, <<"gustafson_motors">>}].
%% [{domain,<<"repository">>},
%% {resource,<<"gustafson_motors">>}]
%% 2> Key = <<"86c4ac12292843b66fdee41e022f8497">>
%% 2> hex_api_auth:test_key(hex_core:default_config(), Params, Key).
%% {ok,{204, ..., nil}}
%% '''
%% @end
-spec test_key(map(), map(),  binary()) -> hex_api:response().
test_key(Repo, Params, Key) ->
    ParamList = maps:to_list(Params),
    URI = ["auth?", hex_api:encode_query_string(ParamList)],
    hex_api:get(Repo#{api_key => Key}, list_to_binary(URI)).
