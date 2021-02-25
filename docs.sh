#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     asdf install erlang 24.0-rc1
#     asdf local erlang 24.0-rc1

rebar3 compile
rebar3 edoc
version=0.7.1
ex_doc "hex_core" $version "_build/default/lib/hex_core/ebin" \
  --source-ref v${version} \
  --config docs.exs $@
