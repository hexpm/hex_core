-module(hex_api_user).
-export([
    create/4,
    get/2,
    me/1,
    reset_password/2
]).

me(Options) when is_map(Options) ->
    hex_api:get(<<"/users/me">>, Options).

create(Username, Password, Email, Options) ->
    Params = #{
      <<"username">> => Username,
      <<"password">> => Password,
      <<"email">> => Email
    },
    hex_api:post(<<"/users">>, Params, Options).

reset_password(Username, Options) when is_binary(Username) and is_map(Options) ->
    hex_api:post(<<"/users/", Username/binary, "/reset">>, #{}, Options).

%% @doc
%% Gets user.
%%
%% Examples:
%%
%% ```
%%     hex_api_user:get(<<"user">>, hex_erl:default_options()).
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
get(Username, Options) when is_binary(Username) and is_map(Options) ->
    hex_api:get(<<"/users/", Username/binary>>, Options).
