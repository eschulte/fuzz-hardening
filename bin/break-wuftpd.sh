#!/bin/bash
#
# Usage: break.sh [variant]
#  Generates fuzz variants until one breaks variant
#
VARIANT=$1
BASE=$(dirname $0)
FUZZ=$BASE/fuzz
LIMIT=$BASE/limit60
TRIALS=10000
PORT="9000" # because it makes me laugh, no good reason
FTP="/usr/bin/ftp -in localhost $PORT"

# run the wuftpd server
$LIMIT $VARIANT -s -p $PORT

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
                $LIMIT $FTP < $OUT >/dev/null 2>/dev/null
                if [ $? -gt 1 ];then
                    echo "$OUT $RESULT"
                    # kill the wuftpd server
                    killall `basename $VARIANT`
                    killall -9 `basename $VARIANT`
                    exit 0
                    break
                else
                    rm $OUT
                fi
            done
        done
    done
done

# kill the wuftpd server
killall `basename $VARIANT`
killall -9 `basename $VARIANT`
wait

exit 1
