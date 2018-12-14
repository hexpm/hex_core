# Release process

1. Bump version in `src/hex_core.app.src` and `doc/tpl.html`

2. Ensure CHANGELOG is updated and add current date

3. Commit changes above with title "Release vVERSION"

4. Push master and the new tag

5. Create GitHub release

6. Publish package: `rebar3 hex publish`

7. Spot check docs: `rebar3 as docs edoc`

8. Publish docs: `rebar3 hex docs`
