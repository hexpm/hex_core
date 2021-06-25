#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     asdf install erlang 24.0-rc3
#     asdf local erlang 24.0-rc3

rebar3 compile
rebar3 edoc
version=0.8.2
ex_doc "hex_core" $version "_build/default/lib/hex_core/ebin" \
  --source-ref v${version} \
  --config docs.config $@
