#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc
#     OTP_GITHUB_URL="https://github.com/erszcz/otp" asdf install erlang ref:edoc-chunk-support
#     asdf local erlang ref:edoc-chunk-support

rebar3 edoc
version=0.7.1
ex_doc "hex_core" $version "_build/default/lib/hex_core/ebin" \
  --source-ref v${version} \
  --config docs.exs $@

# Work around rebar3 docs publishing bug, define fake Mix project:
#
#     # mix.exs
#     defmodule HexCore.MixProject do
#       use Mix.Project
#
#       def project() do
#         [
#           app: :hex_core,
#           version: "0.7.1",
#           aliases: [
#             docs: fn _ ->
#               :ok
#             end
#           ]
#         ]
#       end
#     end
#
# And run:
#
#     mix hex.publish docs
