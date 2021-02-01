#!/bin/bash
set -e

# Setup:
#
#     mix escript.install github elixir-lang/ex_doc branch wm-erl
#     OTP_GITHUB_URL="https://github.com/erszcz/otp" asdf install erlang ref:edoc-chunk-support
#     asdf local erlang ref:edoc-chunk-support

rebar3 as docs edoc
ex_doc "hex_core" "0.7.1" "_build/docs/lib/hex_core/ebin" -u "https://github.com/hexpm/hex_core" $@

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
