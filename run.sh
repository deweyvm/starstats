#!/usr/bin/env bash
EXE=./dist/build/ircdb/ircdb

pushd src &> /dev/null && \
#echo "Generating documentation..." &&\
#out=`haddock -h -o ../docs Main.hs`
#if [[ $? -ne 0 ]] ; then
#    echo WARNING: haddock failure
#    echo $out
#fi
if [[ "$(expr substr $(uname -s) 1 6)" == "CYGWIN" ]] ; then
    driver="MySQL ODBC 5.3 ANSI Driver"
else
    export MYSQL_UNIX_PORT=/var/run/mysqld/mysqld.sock
    driver="MySql ODBC 5.1 Driver"
fi
popd &> /dev/null
echo "Building... "
rm -f $EXE &&\
cabal build &> /dev/null
if [[ $? -ne 0 ]] ; then
    echo "Build Failed"
    exit 1
fi
log=`cat config`
db=`cat config | sed 's/.*\/#\(.*\)[.]log/\1/'`
echo "Running ircdb... "  &&\
rm -f temp && \
case $1 in
"-p")
    python3 watch.py "$log" "-r" | $EXE "$driver" "$db" "$@" +RTS -K100M -M3.9G #
;;
"-g")
    $EXE "$driver" "$db" +RTS -K100M -M3.9G
;;
"-s")
    $EXE "$driver" "$db" +RTS -K100M -M3.9G > generated.html
;;
"-pc")
    cat "$log" | $EXE "$driver" "$db" +RTS -K100M -M3.9G > generated.html
;;
esac
if [[ $? -ne 0 ]] ; then
    echo "exe failed"
    exit 1
fi
