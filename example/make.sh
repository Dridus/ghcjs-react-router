#! /bin/bash

set -e
set -x

stack build

jsexe=""

mkdir -p js
for f in lib out rts runmain; do
  cp -v .stack-work/install/*/*/ghcjs-0.2.0_ghc-7.10.3/bin/example.jsexe/$f.js js/
done
cp ../js/ReactRouter.js js/

