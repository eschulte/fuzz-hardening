#!/bin/bash
#
# Usage: indent-test.sh [indent-variant]
# Test the indent executable on known good tests.
#
VARIANT=$1
BASE=$(dirname $0)
IND_BASE=$BASE/indent
LIMIT=$BASE/limit

run(){
    diff <($LIMIT $VARIANT < $IND_BASE/t${1} 2>&1) $IND_BASE/output.t${1} \
        >/dev/null 2>/dev/null; }

COUNT=0
for i in {1..5};do
    if run $i;then COUNT=$(($COUNT + 1)); fi
done
echo "$COUNT"
