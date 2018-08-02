# hex_erl

[![Build Status](https://travis-ci.org/hexpm/hex_erl.svg?branch=master)](https://travis-ci.org/hexpm/hex_erl)

Reference implementation of Hex specifications: https://github.com/hexpm/specifications.

## Usage

Let's use default options for now. See "Configuration" section below for customization.

```
Options = hex_erl:default_options().
```

Get all package names:

```erlang
hex_repo:get_names(Options).
%%=> {ok, {200, ...,
%%=>     #{package => [
%%=>         #{name => <<"package1">>},
%%=>         #{name => <<"package2">>},
%%=>         ...
%%=>     ]}}}
```

Get all package versions from repository:

```erlang
hex_repo:get_versions(Options).
%%=> {ok, {200, ...,
%%=>     #{packages => [
%%=>         #{name => <<"package1">>, retired => [], versions => [<<"1.0.0">>]},
%%=>         #{name => <<"package2">>, retired => [], versions => [<<"0.5.0">>]},
%%=>     ]}}}
```

Get package releases from repository:

```erlang
hex_repo:get_package(<<"package1">>, Options).
%%=> {ok, {200, ...,
%%=>     #{releases => [
%%=>         #{checksum => ..., version => <<"0.5.0">>, dependencies => []}],
%%=>         #{checksum => ..., version => <<"1.0.0">>, dependencies => []}],
%%=>     ]}}}
```

Get package from HTTP API:

```erlang
hex_api:get_package(<<"package1">>, Options).
%%=> {ok, {200, ...,
%%=>     #{
%%=>         <<"name">> => <<"package1">>,
%%=>         <<"meta">> => #{
%%=>            <<"description">> => ...,
%%=>            <<"licenses">> => ...,
%%=>            <<"links">> => ...,
%%=>            <<"maintainers">> => ...,
%%=>         },
%%=>         ...,
%%=>         <<"releases">> => [
%%=>             #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%=>             #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%=>             ...
%%=>         ]
%%=>     }}}
```

Get package tarball:

```erlang
{ok, {200, _, Tarball}} = hex_repo:get_tarball(<<"package1">>, <<"1.0.0">>, Options).
```

Unpack package tarball:

```erlang
{ok, #{checksum := Checksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
```

Create package tarball:

```erlang
{ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Contents).
```

## Configuration

The default configuration, provided by `hex_erl:default_options/0`, uses built-in httpc-based adapter and Hex.pm APIs:
<https://hex.pm/api> and <https://repo.hex.pm>.

HTTP client configuration can be overriden as follows:

```erlang
HackneyClient = #{
    adapter => my_hackney_adapter,
    user_agent_fragment => <<"(my_app/0.1.0) (hackney/1.12.1) ">>
},
Options = hex_erl:default_options(),
Options2 = lists:keystore(client, 1, Options, {client, HackneyClient}),
hex_repo:get_names(Options2).

%% my_hackney_adapter.erl
-module(my_hackney_adapter).
-behaviour(hex_http).
-exports([request/3]).

request(Method, URI, ReqHeaders) ->
    %% ...
```

## Wrapper Module

It's recommended to write a wrapper module because a lot of decisions are left to the user, e.g.:
where to get configuration from, how to handle caching, failures etc.

For a sample, see: [`examples/myapp_hex.erl`](examples/myapp_hex.erl). Here's an excerpt:

```erlang
-module(myapp_hex).
-export([
    get_api_package/1,
    get_repo_tarball/2,
    get_repo_versions/0
]).

%%====================================================================
%% API functions
%%====================================================================

get_api_package(Name) ->
      case hex_api:get_package(Name, options()) of
          {ok, {200, _Headers, Payload}} ->
              {ok, Payload};

          Other ->
              Other
      end.

get_repo_versions() ->
      case hex_repo:get_versions(options()) of
          {ok, {200, _Headers, Payload}} ->
              {ok, maps:get(packages, Payload)};

          Other ->
              Other
      end.

%%====================================================================
%% Internal functions
%%====================================================================

options() ->
    Options1 = hex_erl:default_options(),
    Options2 = put_client(Options1),
    Options3 = maybe_put_api_key(Options2),
    Options3.

put_client(Options) ->
    Client1 = proplists:get_value(client, Options),
    Client2 = maps:put(user_agent_fragment, <<"(myapp/1.0.0) (httpc)">>, Client1),
    lists:keystore(client, 1, Options, {client, Client2}).

maybe_put_api_key(Options) ->
    case os:getenv("HEX_API_KEY") of
        false -> Options;
        Key -> lists:keystore(api_key, 1, Options, {api_key, Key})
    end.
```

## Installation

### Rebar3

Add to `rebar.config`:

```erlang
{deps, [
  {hex_erl, {git, "git://github.com/hexpm/hex_erl.git"}}
]}
```

### Mix

Add to `mix.exs`:

```elixir
defp deps do
  [
    {:hex_erl, github: "hexpm/hex_erl"}
  ]
end
```

## Development

* Run `rebar3 as dev compile` to re-generate protobuf files
* Run `rebar3 as test proper` for property-based tests
