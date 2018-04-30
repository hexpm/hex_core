# hex_erl

[![Build Status](https://travis-ci.org/hexpm/hex_erl.svg?branch=master)](https://travis-ci.org/hexpm/hex_erl)

Reference implementation of Hex specifications: https://github.com/hexpm/specifications.

## Usage

Get all package names:

```erlang
hex_repo:get_names().
%%=> {ok, #{
%%=>     package => [
%%=>         #{name => <<"package1">>},
%%=>         #{name => <<"package2">>},
%%=>         ...
%%=>     ]
%%=> }}
```

Note: By default, we fetch data from repo.hex.pm using built-in httpc-based adapter.
See section below about configuring HTTP client.

Get all package versions from repository:

```erlang
hex_repo:get_versions().
%%=> {ok, #{
%%=>     packages => [
%%=>         #{name => <<"package1">>, retired => [], versions => [<<"1.0.0">>]},
%%=>         #{name => <<"package2">>, retired => [], versions => [<<"0.5.0">>]},
%%=>     ]
%%=> }}
```

Get package releases from repository:

```erlang
hex_repo:get_package(<<"package1">>).
%%=> {ok, #{
%%=>     releases => [
%%=>         #{checksum => ..., version => <<"0.5.0">>, dependencies => []}],
%%=>         #{checksum => ..., version => <<"1.0.0">>, dependencies => []}],
%%=>     ]
%%=> }}
```

Get package from HTTP API:

```erlang
hex_api:get_package(<<"package1">>).
%%=> {ok, #{
%%=>     <<"name">> => <<"package1">>,
%%=>     <<"meta">> => #{
%%=>        <<"description">> => ...,
%%=>        <<"licenses">> => ...,
%%=>        <<"links">> => ...,
%%=>        <<"maintainers">> => ...,
%%=>     },
%%=>     ...,
%%=>     <<"releases">> => [
%%=>         #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%=>         #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%=>         ...
%%=>     ]}}
```

Get package tarball:

```erlang
{ok, Tarball, _Opts} = hex_repo:get_tarball(<<"package1">>, <<"1.0.0">>).
```

Unpack package tarball:

```erlang
{ok, #{checksum := Checksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
```

Create package tarball:

```erlang
{ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Contents).
```

## Configuring HTTP client

By default, `hex_repo` and `hex_api` functions are using built-in httpc-based adapter and are calling
<https://repo.hex.pm> and <https://hex.pm/api> respectively.

See `hex_repo:default_options()` and `hex_api:default_options()` for available configuration options.

HTTP client configuration can be overriden as follows:

```erlang
Options = [{client, #{adapter => my_hackney_adapter, user_agent_fragment => <<"(hackney/1.12.1) (my_app/0.1.0)">>}}].
hex_repo:get_names(Options).

%% my_hackney_adapter.erl
-module(my_hackney_adapter).
-behaviour(hex_http).
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
