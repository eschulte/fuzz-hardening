#!/bin/bash
#
# Usage: test-wuftpd[wuftpd-variant]
# Test the wu-ftpd executable on known good tests.
#
VARIANT=$1
BASE=$(dirname $0)
IND_BASE=$BASE/wuftpd
LIMIT=$BASE/limit60
PORT="9000" # because it makes me laugh, no good reason
FTP="/usr/bin/ftp -in localhost $PORT"
NC="/usr/bin/nc localhost $PORT"

runftp(){
    diff <($LIMIT $FTP < $IND_BASE/ftp-command-${1}.out 2>&1) $IND_BASE/ftp-command-${1}.out \
        >/dev/null 2>/dev/null; }

runnc() {
    diff <($LIMIT $NC < $IND_BASE/nc-command-${1}.out 2>&1) $IND_BASE/nc-command-${1}.out \
        >/dev/null 2>/dev/null; }

COUNT=0
for i in {1..3};do
    if runftp $i;then COUNT=$(($COUNT + 1)); fi
done
for i in {4..5};do
    if runnc $i;then COUNT=$(($COUNT + 1)); fi
done

echo "$COUNT"
