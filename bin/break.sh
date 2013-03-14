#!/bin/bash
#
# Usage: break.sh [variant] [options...]
#  Generates fuzz inputs until one breaks variant
#
# Options:
#  -h,--help          show help information and exit
#  -f,--fuzz          a script to run the fuzz test
#  -l,--limit         a different limit executable
#                     (e.g., limit60 for longer running times)
#  -t,--trials        number of seeds to try before quitting
#
HELP_TEXT=$(cat "$0" \
    |sed '/^[^#]/q' \
    |head -n -1 \
    |tail -n +3 \
    |sed -e :a -e '/^\n*$/{$d;N;ba' -e '}' \
    |cut -c3-)
BASE=$(dirname $0)
FUZZ=$BASE/fuzz
if [ -z "$LIMIT" ];then LIMIT=$BASE/limit;fi
TRIALS=10000

## Option Parsing
eval set -- $(getopt -o hf:l:t: -l help,fuzz:,limit:,trials: -- "$@")
while [ $# -gt 0 ];do
    case $1 in
        -h|--help)   echo "$HELP_TEXT">&2;exit 1;;
        -f|--fuzz)   FUZZ="$2";   shift;;
        -l|--limit)  LIMIT="$2";  shift;;
        -t|--trials) TRIALS="$2"; shift;;
        (--) shift; break;;
        (-*) echo "unrecognized option $1">&2; exit 1;;
        (*)  break;;
    esac
    shift
done

## Main Loop
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
                cat $OUT|$LIMIT $@ >/dev/null 2>/dev/null
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
