%% @doc
%% Hex HTTP API - Short URLs.
-module(hex_api_short_url).
-export([create/2]).

%% @doc
%% Creates a short URL.
%%
%% Examples:
%%
%% ```
%% > hex_api_short_url:create(hex_core:default_config(), <<"https://hex.pm/packages/example">>).
%% {ok, {201, ..., #{<<"url">> => <<"https://hex.pm/l/XXXXX">>}}}
%% '''
%% @end
-spec create(hex_core:config(), binary()) -> hex_api:response().
create(Config, URL) when is_map(Config) and is_binary(URL) ->
    Body = #{<<"url">> => URL},
    hex_api:post(Config, ["short_url"], Body).
