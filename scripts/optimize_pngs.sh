#!/bin/bash

echo "Optimizing textures."
for i in `find . -name "*.png" -type f`; do
  optipng $i
done
echo "Done."