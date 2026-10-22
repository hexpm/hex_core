#!/bin/bash

# Checks that every place that names hex_core's version names the version in
# src/hex_core.app.src

set -euo pipefail

version=$(erl -noshell -eval '{ok, [{application, hex_core, App}]} = file:consult("src/hex_core.app.src"), io:put_chars(proplists:get_value(vsn, App)), halt().')
failed=false

# Every line of $1 that matches the extended regex $2 must capture the version
# in the regex's first group, and at least one line must match
check() {
  local file=$1 regex=$2 found
  found=$(sed -nE "s/.*$regex.*/\\1/p" "$file")

  if [ -z "$found" ]; then
    echo "::error file=$file::$file has no line matching $regex"
    failed=true
  fi

  for found_version in $found; do
    if [ "$found_version" != "$version" ]; then
      echo "::error file=$file::$file names version $found_version, src/hex_core.app.src has $version"
      failed=true
    fi
  done
}

check src/hex_core.hrl '^-define\(HEX_CORE_VERSION, "([^"]*)"\)\.$'
check README.md '\{hex_core, "([^"]*)"\}'
check README.md '\{:hex_core, "~> ([^"]*)"\}'

if [ "$failed" = true ]; then
  exit 1
fi

echo "Version $version matches in every file"
