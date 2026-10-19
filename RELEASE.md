# Release process

1. Bump version (see below for all files)

2. Ensure CHANGELOG is updated and add current date

3. Spot check docs: `rebar3 ex_doc`

4. Commit changes above with title "Release v$VERSION"

5. Push main and the new tag `v$VERSION`

Pushing the tag runs the [Release workflow](.github/workflows/release.yml) in the `release` environment. It publishes the package and docs to Hex with Workload Identity and creates the GitHub release with the version's CHANGELOG section as its notes. To publish by hand instead, run `MIX_EXS=publish.exs mix hex.publish`.

## Places where version is mentioned

```
src/hex_core.app.src
src/hex_core.hrl
README.md
```
