#!/bin/bash
#
# Usage test-fuzz.sh [variant] [fuzz-file]
# Run indent against a fuzz variant.
#
LIMIT=$(dirname $0)/limit60
VARIANT=$1
FUZZFILE=$2
PORT="9000" # because it makes me laugh, no good reason
FTP="/usr/bin/ftp -in localhost $PORT"

# run the wuftpd server
$LIMIT $VARIANT -s -p $PORT

# send it some crap via FTP
$FTP < $FUZZFILE >/dev/null 2>/dev/null

# kill the wuftpd server
killall `basename $VARIANT`
killall -9 `basename $VARIANT`
wait
exit $?
