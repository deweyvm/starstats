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
logtype=$4
echo "Running starstats... "  &&\
case $1 in
"--repop")
    $EXE --driver="$driver" \
         --db="$db" \
         --logtype="$logtype" \
         --log="$log" \
         --insert
;;
"--watch")
    $EXE --driver="$driver" \
         --db="$db" \
         --logtype="$logtype" \
         --log="$log" \
         --watch
;;
"--init")
    echo "create database if not exists $db;" | mysql &&\
    $EXE --driver="$driver" \
         --db="$db" \
         --init
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
