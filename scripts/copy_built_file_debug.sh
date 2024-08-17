#!/bin/bash
location=$(find ./build/ -type f -name "formine")

if [ "$location" == "" ]; then
  echo "Script error, blank!"
  exit 1
fi

./scripts/clear_mod_files.sh

if [ ! -d "./out/" ]; then
  mkdir ./out/
fi

rm ./out/formine_release 2> /dev/null

mv $location ./out/formine_debug

echo "Copied debug binary."

exit 0