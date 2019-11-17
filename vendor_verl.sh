#!/bin/bash
set -e

if [[ -z "$1" || -z "$2" ]]; then
  echo "Usage: vendor.sh SOURCE_DIR PREFIX"
  exit 1
fi

source_dir="${1}/src"
target_dir=`dirname $0`/src
prefix=$2
verl_version=`cat $source_dir/verl.hrl | grep VERL_VERSION | cut -d'"' -f2`

filenames="verl.hrl \
           verl.erl \
           verl_parser.erl"

search_to_replace="verl"

for filename in $filenames; do
  rm -f $target_dir/$prefix$filename
  source_path=$source_dir/$filename
  target_path=$target_dir/$prefix$filename

  echo "%% Vendored from verl v$verl_version, do not edit manually" > $target_path
  echo >> $target_path
  cat $source_path >> $target_path

  for word in $search_to_replace; do
    sed -i.bak s/$word/$prefix$word/g $target_path
    rm $target_path.bak
  done
done
