#!/usr/bin/env bash

# Reinstall library
cd ../..
cabal install -p
cd "profiling/Restaurants/"

# Compile test file
ghc -rtsopts -prof -auto-all -fforce-recomp -O2 "Restaurants.hs"

# Run with profiling enabled and view output
./Restaurants +RTS -K100M -s -p # -hy
cat "Restaurants.prof"
# hp2ps -c "Restaurants.hp"
# open "Restaurants.ps"

# Clean up files from last compilation
rm *.hi *.o *.aux
rm Restaurants