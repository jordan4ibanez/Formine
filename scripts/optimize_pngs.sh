#!/bin/bash

echo "Optimizing textures."
for i in `find ./mods/ -name "*.png" -type f`; do
  optipng $i
done
echo "Done."