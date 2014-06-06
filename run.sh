#!/usr/bin/env bash
EXE=./dist/build/starstats/starstats
SIMUL=../ircsimul/ircsimul.py
PY3=python3
if [[ "$(expr substr $(uname -s) 1 6)" == "CYGWIN" ]] ; then
    driver="MySQL ODBC 5.3 ANSI Driver"
else
    export MYSQL_UNIX_PORT=/var/run/mysqld/mysqld.sock
    driver="MySql ODBC 5.1 Driver"
fi

log=$2
db=`echo $log | sed 's/.*\/#\(.*\)[.]log/\1/'`
echo "Running starstats... "  &&\
case $1 in
"--repop")
    $EXE "$driver" "$db" "$log" "-rp" | $EXE "$driver" "$db" "-rd"
;;
"--repop-dry-run")
    $EXE "$driver" "$db" "$log" "-rp"
;;
"--recover")
    $EXE "$driver" "$db" "$log" "-rv" | $EXE "$driver" "$db" "-rd"
;;
"--recover-dry-run")
    $EXE "$driver" "$db" "$log" "-rv"
;;
"--generate")
    $EXE "$driver" "$db" "-g"
;;
"--contrive-repop")
    $PY3 $SIMUL -l -1 --realtime --stdout | $EXE "$driver" "zarathustra" "-rd"
;;
*)
    echo "Unknown option \"$1\""
    exit 1
;;
esac
if [[ $? -ne 0 ]] ; then
    echo "exe failed"
    exit 1
fi
