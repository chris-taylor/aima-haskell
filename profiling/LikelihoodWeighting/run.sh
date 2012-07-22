#!/bin/sh

bash cleanup.sh
ghc -O2 likelihoodWeighting.hs -rtsopts -prof -auto-all
./likelihoodWeighting +RTS -s -p -hy
cat likelihoodWeighting.prof
hp2ps -c likelihoodWeighting.hp
open likelihoodWeighting.ps
