#!/bin/bash
location=$(find ./build/ -type f -name "Formine")

if [ "$location" == "" ]; then
  echo "Script error, blank!"
  exit 1
fi

./scripts/clear_mod_files.sh

if [ ! -d "./out/" ]; then
  mkdir ./out/
fi

rm -f ./out/formine_debug

cp -f $location ./out/

# Bash is extremely annoying.
sleep 0.2

mv ./out/formine ./out/formine_debug

echo "Copied debug binary."

exit 0