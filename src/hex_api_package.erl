-module(hex_api_package).
-export([get/2, search/3]).

%% @doc
%% Gets package.
%%
%% Examples:
%%
%% ```
%%     hex_api_package:get(<<"package">>, hex_core:default_options()).
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
get(Name, Options) when is_binary(Name) and is_map(Options) ->
    hex_api:get(["packages", Name], Options).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%%     hex_api_package:search(<<"package">>, [], hex_core:default_options()).
%%     %%=> {ok, {200, ..., [
%%     %%=>     #{<<"name">> => <<"package1">>, ...},
%%     %%=>     ...
%%     %%=> ]}}
%% '''
search(Query, SearchParams, Options) when is_binary(Query) and is_list(SearchParams) and is_map(Options) ->
    QueryString = hex_api:encode_query_string([{search, Query} | SearchParams]),
    hex_api:get(<<"/packages?", QueryString/binary>>, Options).
