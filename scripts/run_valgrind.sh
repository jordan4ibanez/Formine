#!/bin/bash

MALLOC_CHECK_=2 fpm build --flag -g --flag -lmcheck --c-flag -g --c-flag -lmcheck

location=$(find ./build/ -type f -name "Formine")

if [ "$location" == "" ]; then
  echo "Script error, blank!"
  exit 1
fi
	

valgrind --leak-check=full \
        --show-leak-kinds=all \
        --track-origins=yes \
        --verbose \
        --fair-sched=yes \
        $location


exit 0