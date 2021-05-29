#!/bin/bash
set -e

# You can explicitly set the C compiler by providing a CC environment variable.
# This also works inline: CC=clang ./build.sh
CC="${CC:-cc}"

FLAGS="-std=c11 -g -O0 -pthread -Wno-incompatible-pointer-types"

if [[ $($CC -v 2>&1) == *"clang version"* ]]
then
  FLAGS="$FLAGS -Wno-tautological-constant-out-of-range-compare -Wno-initializer-overrides"
fi

rm -rf build
mkdir -p build

$CC $FLAGS meta.c -o build/meta -lm

cd build
./meta
cd ..

$CC $FLAGS mass.c -o build/mass -lm

$CC $FLAGS source_spec.c -o build/source_spec -lm