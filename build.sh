#!/bin/bash
set -e

FLAGS="-std=c11 -g -O0 -pthread"

if [[ $(cc -v 2>&1) == *"clang version"* ]]
then
  FLAGS="-std=c11 -g -O0 -pthread -Wno-tautological-constant-out-of-range-compare -Wno-initializer-overrides"
fi

mkdir -p build

cc $FLAGS generate_types.c -o build/generate_types -lm

# cd build
# ./generate_types
# cd ..

cc $FLAGS function_spec.c -o build/function_spec -lm