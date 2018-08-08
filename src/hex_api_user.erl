-module(hex_api_user).
-export([
    create/4,
    get/2,
    me/1,
    reset_password/2
]).

me(Config) when is_map(Config) ->
    hex_api:get(Config, ["users", "me"]).

create(Config, Username, Password, Email) ->
    Params = #{
      <<"username">> => Username,
      <<"password">> => Password,
      <<"email">> => Email
    },
    hex_api:post(Config, ["users"], Params).

reset_password(Username, Config) when is_binary(Username) and is_map(Config) ->
    hex_api:post(Config, ["users", Username, "reset"], #{}).

%% @doc
%% Gets user.
%%
%% Examples:
%%
%% ```
%%     hex_api_user:get(<<"user">>, hex_core:default_config()).
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
get(Config, Username) when is_binary(Username) and is_map(Config) ->
    hex_api:get(Config, ["users", Username]).
