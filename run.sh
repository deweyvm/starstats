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


db=$2
log=$3
echo "Running starstats... "  &&\
case $1 in
"--repop")
    $EXE --driver="$driver" \
         --db="$db" \
         --repopulate="$log" | \
    $EXE --driver="$driver" \
         --db="$db" \
         "--read"
;;
"--repop-dry-run")
    $EXE --driver="$driver" --db="$db" --repopulate"=$log"
;;
"--recover")
    $EXE --driver="$driver" --db="$db" --recover="$log" | $EXE --driver="$driver" --db="$db" "--read"
;;
"--recover-dry-run")
    $EXE --driver="$driver" --db="$db" --recover="$log"
;;
"--contrive-repop")
    $PY3 $SIMUL -l -1 --realtime --stdout | $EXE --driver="$driver" --db="zarathustra" "--read"
;;
"--contrive-1-year")
    $PY3 $SIMUL -a 1200 -d 365 -m --stdout | $EXE "--driver=$driver" "--db=zarathustra" "--read"
;;
"--init")
    $EXE  "--driver=$driver" "--channel=$channel" "--server=$server" "--db=$db" "--init"
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
