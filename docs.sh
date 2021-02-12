#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     asdf install erlang ref:master
#     asdf local erlang ref:master

rebar3 compile
rebar3 edoc
version=0.7.1
ex_doc "hex_core" $version "_build/default/lib/hex_core/ebin" \
  --source-ref v${version} \
  --config docs.exs $@
