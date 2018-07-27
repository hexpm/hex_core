#!/bin/bash
set -e

if [[ -z "$1" || -z "$2" ]]; then
  echo "Usage: vendor.sh TARGET_DIR PREFIX"
  exit 1
fi

source_dir=`dirname $0`/src
target_dir=$1
prefix=$2
hex_erl_version=`cat $source_dir/hex_erl.hrl | grep HEX_ERL_VERSION | cut -d'"' -f2`

filenames="hex_erl.hrl \
           hex_erl_tar.erl \
           hex_erl_tar.hrl \
           hex_filename.erl \
           hex_pb_package.erl \
           hex_pb_signed.erl \
           hex_tarball.erl \
           hex_registry.erl \
           hex_http_httpc.erl \
           hex_http.erl \
           hex_repo.erl \
           hex_api.erl \
           safe_erl_term.xrl"

search_to_replace="hex_erl.hrl \
                   hex_erl_tar \
                   hex_filename \
                   hex_pb_package \
                   hex_pb_signed \
                   hex_registry \
                   hex_tarball \
                   hex_http \
                   hex_repo \
                   hex_api \
                   safe_erl_term"

rm -f $target_dir/$prefix*

for filename in $filenames; do
  source_path=$source_dir/$filename
  target_path=$target_dir/$prefix$filename

  echo "%% Vendored from hex_erl v$hex_erl_version, do not edit manually" > $target_path
  echo >> $target_path
  cat $source_path >> $target_path

  for word in $search_to_replace; do
    sed -i.bak s/$word/$prefix$word/g $target_path
    rm $target_path.bak
  done
done
