# Release process

1. Bump version (see below for all files)

2. Ensure CHANGELOG is updated and add current date

3. Commit changes above with title "Release v$VERSION"

4. Push master and the new tag

5. Create GitHub release: `open "https://github.com/hexpm/hex_core/releases/new?title=v$VERSION&tag=v$VERSION&body=content%0Afrom%0Achangelog"`

6. Publish package: `rebar3 hex publish`

7. Spot check docs: `rebar3 as docs edoc`

8. Publish docs: `rebar3 hex docs`

## Places where version is mentioned

```
src/hex_core.app.src
doc/tpl.html
src/hex_core.hrl
```
