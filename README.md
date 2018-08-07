# hex_core

[![Build Status](https://travis-ci.org/hexpm/hex_core.svg?branch=master)](https://travis-ci.org/hexpm/hex_core)

Reference implementation of Hex specifications: https://github.com/hexpm/specifications.

## Usage

Let's use default options for now. See "Configuration" section below for customization.

```
Options = hex_core:default_options().
```

Get all package names:

```erlang
hex_repo:get_names(Options).
%%=> {ok, {200, ...,
%%=>     #{packages => [
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
hex_api_package:get(<<"package1">>, Options).
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

The default configuration, provided by `hex_core:default_options/0`, uses built-in httpc-based adapter and Hex.pm APIs:
<https://hex.pm/api> and <https://repo.hex.pm>.

Organizations on Hex.pm (or any compatible server) can be configured as following:

```erlang
Options = maps:merge(hex_core:default_options(), #{
    api_key => APIKey,
    organization => <<"acme">>,
    repo_key => RepoKey,
    repo_url => <<"https://repo.hex.pm/repos/acme">>
}).
```

HTTP client configuration can be overriden as follows:

```erlang
Options = maps:merge(hex_core:default_options(), #{
  http_adapter => my_hackney_adapter,
  http_user_agent_fragment => <<"(my_app/0.1.0) (hackney/1.12.1) ">>
}),
hex_repo:get_names(Options).

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
      case hex_api_package:get(Name, options()) of
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
    Options1 = hex_core:default_options(),
    Options2 = put_http_options(Options1),
    Options3 = maybe_put_api_key(Options2),
    Options3.

put_http_options(Options) ->
    maps:put(http_user_agent_fragment, <<"(myapp/1.0.0) (httpc)">>, Options).

maybe_put_api_key(Options) ->
    case os:getenv("HEX_API_KEY") of
        false -> Options;
        Key -> maps:put(api_key, Key, Options)
    end.
```

## Installation

### Rebar3

Add to `rebar.config`:

```erlang
{deps, [
  {hex_core, {git, "git://github.com/hexpm/hex_core.git"}}
]}
```

### Mix

Add to `mix.exs`:

```elixir
defp deps do
  [
    {:hex_core, github: "hexpm/hex_core"}
  ]
end
```

## Development

* Run `rebar3 as dev compile` to re-generate protobuf files
* Run `rebar3 as test proper` for property-based tests
