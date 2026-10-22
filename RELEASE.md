# Release process

1. Bump version (see below for all files)

2. Ensure CHANGELOG is updated and add current date

3. Spot check docs: `rebar3 ex_doc`

4. Commit changes above with title "Release v$VERSION"

5. Push main

Every push to main runs the [Release workflow](.github/workflows/release.yml) in the `release` environment. When the version in `src/hex_core.app.src` hasn't been released yet, it tags the pushed commit `v$VERSION`, publishes the package and docs to Hex with Workload Identity and creates the GitHub release with the version's CHANGELOG section as its notes. Each step is skipped when it's already done, so re-running a failed run picks up where it stopped. To publish by hand instead, run `MIX_EXS=publish.exs mix hex.publish`.

## Places where version is mentioned

```
src/hex_core.app.src
src/hex_core.hrl
README.md
```

[`check_version.sh`](.github/workflows/check_version.sh) fails when `src/hex_core.hrl` or `README.md` names a different version than `src/hex_core.app.src`. CI runs it, and the Release workflow runs it before tagging.
