#!/bin/bash
set -e

./build.sh

for file in build/*_spec
do
  ASAN_OPTIONS=detect_leaks=0,detect_stack_use_after_return=1 $file
done
