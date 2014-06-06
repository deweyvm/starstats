#!/usr/bin/env bash
EXE=./dist/build/starstats/starstats

echo "Building... "
rm -f $EXE &&\
cabal build 2>&1 >/dev/null
if [[ $? -ne 0 ]] ; then
    echo "Build Failed"
    exit 1
fi
