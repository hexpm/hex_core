#!/bin/bash
set -e

cd $(dirname $(realpath $0))

cp ../README.md index.md

make clean
make

# fix internal doc links
sed -E -i.bak 's/\"([a-zA-Z_-]*)\.md([a-zA-Z#_-]*)\"/\"\1.html\2\"/g' *.html
sed -E -i.bak 's/\"doc\/([a-zA-Z_-]*)\.md\"/\"\1.html\"/g' *.html
sed -E -i.bak 's/\"([a-zA-Z_-]*)\.md\"/\"\1.html\"/g' *.html
sed -E -i.bak 's/<br \/>//g' *.html

# fix external doc links
sed -E -i.bak 's/maps\.html\#/http:\/\/erlang.org\/doc\/man\/maps\.html#/g' *.html
sed -E -i.bak 's/public_key\.html\#/http:\/\/erlang.org\/doc\/man\/public_key\.html#/g' *.html
sed -E -i.bak 's/unicode\.html\#/http:\/\/erlang.org\/doc\/man\/unicode\.html#/g' *.html
sed -E -i.bak 's/maps\.md\#/http:\/\/erlang.org\/doc\/man\/maps\.html#/g' *.html
sed -E -i.bak 's/unicode\.md\#/http:\/\/erlang.org\/doc\/man\/unicode\.html#/g' *.html

rm *.bak
