#!/bin/bash
#
# Usage: break.sh [variant]
#  Generates fuzz variants until one breaks variant
#
VARIANT=$1
BASE=$(dirname $0)
FUZZ=$BASE/fuzz
LIMIT=$BASE/limit
TRIALS=10000

for SEED in $(seq $TRIALS);do
    for NUM in 1000 10000 100000;do
        for NULL in 0 1;do
            for PRINTABLE in 0 1;do

                # Generate Fuzz Output
                OUT=$(mktemp)
                if [ $NULL -eq 0 ];then
                    if [ $PRINTABLE -eq 0 ];then
                        $FUZZ $NUM -s $SEED       -o $OUT >/dev/null 2>/dev/null
                    else
                        $FUZZ $NUM -s $SEED -p    -o $OUT >/dev/null 2>/dev/null
                    fi
                else
                    if [ $PRINTABLE -eq 0 ];then
                        $FUZZ $NUM -s $SEED    -0 -o $OUT >/dev/null 2>/dev/null
                    else
                        $FUZZ $NUM -s $SEED -p -0 -o $OUT >/dev/null 2>/dev/null
                    fi
                fi

                # Run the Tests
                cat $OUT|$LIMIT $VARIANT >/dev/null 2>/dev/null
                if [ $? -gt 1 ];then
                    echo "$OUT $RESULT"
                    exit 0
                    break
                else
                    rm $OUT
                fi
            done
        done
    done
done
exit 1
