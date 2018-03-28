# hex_erl

[![Build Status](https://travis-ci.org/hexpm/hex_erl.svg?branch=master)](https://travis-ci.org/hexpm/hex_erl)

Reference implementation of Hex specifications: https://github.com/hexpm/specifications.

## Usage

Get all package names:

```erlang
hex_repo:get_names().
%%=> {ok, #{package => [
%%=>     #{name => <<"package1">>},
%%=>     #{name => <<"package2">>},
%%=>     ...]}}
```

Note: By default, we fetch data from repo.hex.pm using built-in httpc-based adapter.
See `hex_repo:default_options()` for available configuration options.
Custom HTTP adapter must implement `hex_http` behaviour.

Get all package versions:

```erlang
hex_repo:get_versions().
%%=> {ok, #{packages => [
%%=>     #{name => <<"package1">>, retired => [],
%%=>       versions => [<<"1.0.0">>]},
%%=>     #{name => <<"package2">>, retired => [],
%%=>       versions => [<<"0.5.0">>]},
%%=>     ...]}}
```

Get package releases:

```erlang
hex_repo:get_package("package1").
%%=> {ok, #{releases => [
%%=>     #{checksum => ..., version => <<"0.5.0">>, dependencies => []}],
%%=>     #{checksum => ..., version => <<"1.0.0">>, dependencies => []}],
%%=>     ...]}}
```

Get package tarball:

```erlang
{ok, Tarball} = hex_repo:get_tarball("package1", "1.0.0").
```

Unpack package tarball:

```erlang
{ok, #{checksum := Checksum, contents := Contents, metadata := Metadata}} = hex_tarball:unpack(Tarball, memory).
```

Create package tarball:

```erlang
{ok, {Tarball, Checksum}} = hex_tarball:create(Metadata, Contents).
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
