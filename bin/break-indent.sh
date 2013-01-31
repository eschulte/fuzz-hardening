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

make_fuzz(){
    local seed=$1; local output=$(mktemp);
    $FUZZ -s $1 -o $output $FUZZ_SIZE >/dev/null 2>/dev/null;
    echo $output; }

echo -n "checking 1000 variants "
for SEED in {0..1000};do
    FUZZ_FILE=$(make_fuzz $SEED)
    $FUZZ_TEST $VARIANT $FUZZ_FILE >/dev/null 2>/dev/null
    RESULT=$?
    if [ ! $? -eq 0 ];then
        echo "$FUZZ_FILE broke $VARIANT returning $RESULT"
        break;
    else
        echo -n "."
        rm $FUZZ_FILE
    fi
done
echo ""
