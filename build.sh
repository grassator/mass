#!/bin/bash
set -e

function compile {
  if cc -v 2>&1 | grep -c "clang version"
  then
    cc -std=c17 -g -O0 -pthread \
      -Wno-tautological-constant-out-of-range-compare \
      -Wno-initializer-overrides \
      $1.c -o build/$1 \
      -lm
  else
    # TODO cleanup to use -Wall
    cc -std=c17 -g -O0 -pthread \
      $1.c -o build/$1 \
      -lm
  fi
}



mkdir -p build

compile generate_types

# cd build
# ./generate_types
# cd ..

compile function_spec