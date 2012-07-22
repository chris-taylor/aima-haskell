#!/usr/bin/env bash

# Reinstall library
cd ../..
cabal install -p -O2
cd "profiling/"$1

# Compile test file
ghc -rtsopts -prof -fprof-auto -fforce-recomp --make -O2 $1".hs"

# Run with profiling enabled and view output
./$1 +RTS -K100M -s -p -hy
cat $1".prof"
hp2ps -c $1".hp"
open $1".ps"

# Clean up files from last compilation
./cleanup.sh