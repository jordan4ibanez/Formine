#!/bin/bash
location=$(find ./build/ -type f -name "formine")

if [ "$location" == "" ]; then
  echo "Script error, blank!"
  exit 1
fi

cp $location ./out/formine_debug

echo "Copied debug binary."

exit 0