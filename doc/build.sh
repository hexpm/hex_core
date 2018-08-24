#!/bin/bash

cd $(dirname $(realpath $0))

cp ../README.md index.md

make clean
make

# fix internal doc links
sed -i 's/\"\([a-zA-Z_-]*\)\.md\([a-zA-Z_-#]*\)\"/\"\1.html\2\"/g' *.html
sed -i 's/\"doc\/\([a-zA-Z_-]*\)\.md\"/\"\1.html\"/g' *.html
sed -i 's/\"\([a-zA-Z_-]*\)\.md\"/\"\1.html\"/g' *.html
sed -i 's/<br \/>//g' *.html

# fix external doc links
sed -i 's/maps\.html\#/http:\/\/erlang.org\/doc\/man\/maps\.html#/g' *.html
sed -i 's/public_key\.html\#/http:\/\/erlang.org\/doc\/man\/public_key\.html#/g' *.html
sed -i 's/unicode\.html\#/http:\/\/erlang.org\/doc\/man\/unicode\.html#/g' *.html
sed -i 's/maps\.md\#/http:\/\/erlang.org\/doc\/man\/maps\.html#/g' *.html
sed -i 's/unicode\.md\#/http:\/\/erlang.org\/doc\/man\/unicode\.html#/g' *.html

