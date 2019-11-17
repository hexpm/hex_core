#!/bin/bash
set -e

if [[ -z "$1" || -z "$2" ]]; then
  echo "Usage: vendor.sh TARGET_DIR PREFIX"
  exit 1
fi

root_target_dir=`dirname $0`
target_dir=$root_target_dir/src
source_dir=$1/src
source_test_dir=$1/test
target_test_dir=$root_target_dir/test
prefix="hex_core_"
verl_version=`cat $source_dir/verl.hrl | grep VERL_VERSION | cut -d'"' -f2`

filenames="verl.hrl \
           verl.erl \
           verl_parser.erl"

test_filenames="prop_verl.erl \
                verl_SUITE.erl \
                verl_parser_SUITE.erl"

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


for filename in $test_filenames; do
  rm -f $target_test_dir/$prefix$filename
  source_path=$source_test_dir/$filename

  rest="$filename"
  first="${rest%%_*}"; rest="${rest#*_}"

  if [[ "${first}" == "prop" ]]; then
    target_path=$target_test_dir/"prop_${prefix}${rest}"
  else
    target_path=$target_test_dir/$prefix$filename
  fi

  echo "%% Vendored from verl v$verl_version, do not edit manually" > $target_path
  echo >> $target_path
  cat $source_path >> $target_path

  for word in $search_to_replace; do
    sed -i.bak s/$word/$prefix$word/g $target_path
    sed -i.bak s/"Vendored from hex_core_verl"/"Vendored from verl"/g $target_path
    rm $target_path.bak
  done
done
