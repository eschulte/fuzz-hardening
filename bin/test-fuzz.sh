#!/bin/bash
#
# Usage test-fuzz.sh [variant] [fuzz-file]
# Run indent against a fuzz variant.
#
LIMIT=$(dirname $0)/limit
VARIANT=$1
FUZZFILE=$2

$LIMIT $VARIANT < $FUZZFILE >/dev/null 2>/dev/null
exit $?
