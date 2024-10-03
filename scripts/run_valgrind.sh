#!/bin/bash

MALLOC_CHECK_=2 fpm build --flag -g --flag -lmcheck --c-flag -g --c-flag -lmcheck

location=$(find ./build/ -type f -name "Formine")

if [ "$location" == "" ]; then
  echo "Script error, blank!"
  exit 1
fi
	

valgrind --leak-check=full \
         --track-origins=yes \
         --fair-sched=yes \
         $location


exit 0