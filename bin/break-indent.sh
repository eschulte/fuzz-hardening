#!/bin/bash
#
# Usage: break-indent.sh [variant]
#  Generates fuzz variants until one breaks variant
#
VARIANT=$1
BASE=$(dirname $0)
FUZZ=$(dirname $0)/fuzz
FUZZ_TEST=$(dirname $0)/test-fuzz.sh
FUZZ_SIZE=1024 # 4096
TRIALS=1000

make_fuzz(){
    local SEED=$1; local output=$(mktemp);
    # $FUZZ -s $SEED -o $output $FUZZ_SIZE >/dev/null 2>/dev/null;
    $FUZZ -s $SEED -o $output -a >/dev/null 2>/dev/null;
    echo $output; }

echo -n "checking $TRIALS variants"
for SEED in $(seq $TRIALS);do
    FUZZ_FILE=$(make_fuzz $SEED)
    $FUZZ_TEST $VARIANT $FUZZ_FILE >/dev/null 2>/dev/null
    RESULT=$?
    if [ $RESULT -gt 1 ];then
        echo ""
        echo "$FUZZ_FILE $RESULT"
        exit 0
        break
    else
        echo -n "."
        rm $FUZZ_FILE
    fi
done
echo ""
exit 1
