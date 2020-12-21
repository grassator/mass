#!/bin/bash
set -e

FLAGS="-std=c11 -g -O0 -pthread"

if [[ $(cc -v 2>&1) == *"clang version"* ]]
then
  FLAGS="-std=c11 -g -O0 -pthread -Wno-tautological-constant-out-of-range-compare -Wno-initializer-overrides"
fi

rm -rf build
mkdir -p build

cc $FLAGS generate_types.c -o build/generate_types -lm

cd build
./generate_types
cd ..

cc $FLAGS mass.c -o build/mass -lm

cc $FLAGS function_spec.c -o build/function_spec -lm

# We can compile but not yet run source_spec so adding a suffix _norun to the binary
cc $FLAGS source_spec.c -o build/source_spec_norun -lm