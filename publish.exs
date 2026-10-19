# Publishes the package and docs with Mix: `MIX_EXS=publish.exs mix hex.publish`.
#
# It isn't named mix.exs because Mix would then build hex_core with Mix
# instead of rebar3 when it's a git dependency. The package doesn't include
# this file and lists rebar3 as its build tool, so Mix builds it with rebar3.
defmodule HexCore.MixProject do
  use Mix.Project

  {:ok, [{:application, :hex_core, app}]} = :file.consult("src/hex_core.app.src")
  @app app

  def project do
    [
      app: :hex_core,
      version: to_string(@app[:vsn]),
      description: to_string(@app[:description]),
      language: :erlang,
      deps: [],
      package: [
        build_tools: ["rebar3"],
        files: ~w(CHANGELOG.md LICENSE README.md rebar.config rebar.config.script rebar.lock src),
        exclude_patterns: [~r"^src/safe_erl_term\.erl$"],
        licenses: Enum.map(@app[:licenses], &to_string/1),
        links: Map.new(@app[:links], fn {name, url} -> {to_string(name), to_string(url)} end)
      ],
      aliases: [docs: &docs/1]
    ]
  end

  defp docs(_args) do
    {_, 0} = System.cmd("rebar3", ["ex_doc"], into: IO.stream())
  end
end
