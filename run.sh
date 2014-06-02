#!/usr/bin/env bash
EXE=./dist/build/ircdb/ircdb

pushd src &> /dev/null && \
#echo "Generating documentation..." &&\
#out=`haddock -h -o ../docs Main.hs`
#if [[ $? -ne 0 ]] ; then
#    echo WARNING: haddock failure
#    echo $out
#fi
popd &> /dev/null
echo "Building... "
rm -f $EXE &&\
cabal build &> /dev/null
if [[ $? -ne 0 ]] ; then
    echo "Build Failed"
    exit 1
fi
echo "Running ircdb... "  &&\
rm -f temp && \
time $EXE "$@" +RTS -K100M -M3.9G | tee temp | (egrep '^>' || true)
if [[ $? -ne 0 ]] ; then
    echo "exe failed"
    exit 1
fi
cat temp | egrep '^@' | sort -k2 -t '	' &&\
cat temp | egrep '^>' | sed 's/.* .* \(.*\)/\1/g' > "in.csv" &&\
lines=`cat "in.csv" | wc -l`
if [[ "$lines" -gt 0 ]] ; then
    gnuplot graph.plot
fi

if [[ $? -ne 0 ]] ; then
    echo "failed"
    exit 1
fi
