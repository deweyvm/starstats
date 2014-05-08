#!/usr/bin/env bash
EXE=./dist/build/ircdb/ircdb.exe

pushd src &> /dev/null && \
echo "Generating documentation..." &&\
out=`haddock -h -o ../docs Main.hs`
if [[ $? -ne 0 ]] ; then
    echo WARNING: haddock failure
    echo $out
fi
popd &> /dev/null
echo "Building... "
rm -f $EXE &&\
cabal build &> /dev/null &&\
echo "Running ircdb... "  &&\
time $EXE
if [[ $? -ne 0 ]] ; then
    echo "failed"
fi
