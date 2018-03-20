#!/bin/bash
set -e

if [ "$1" = "" ]; then
  echo "Usage: vendor.sh VENDOR_TO_PATH"
  exit 1
fi

original_src=`dirname $0`/src
vendored_src=$1
hex_erl_version=`cat $original_src/hex_erl.hrl | grep HEX_ERL_VERSION | cut -d'"' -f2`

filenames="hex_erl.hrl \
           hex_erl_tar.erl \
           hex_erl_tar.hrl \
           hex_filename.erl \
           hex_pb_package.erl \
           hex_pb_signed.erl \
           hex_tarball.erl \
           hex_registry.erl \
           safe_erl_term.xrl"

search_to_replace="hex_erl.hrl \
                   hex_erl_tar \
                   hex_filename \
                   hex_pb_package \
                   hex_pb_signed \
                   hex_registry \
                   hex_tarball \
                   safe_erl_term"

rm -f $vendored_src/vendored_*

for filename in $filenames; do
  original_path=$original_src/$filename
  vendored_path=$vendored_src/vendored_$filename

  echo "%% Vendored from hex_erl v$hex_erl_version, do not edit manually" > $vendored_path
  echo >> $vendored_path
  cat $original_path >> $vendored_path

  for word in $search_to_replace; do
    sed -i .bak s/$word/vendored_$word/g $vendored_path
    rm $vendored_path.bak
  done
done
