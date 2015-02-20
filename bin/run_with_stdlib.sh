#!/bin/bash
set -e
#set -x

SCRATCH=$(mktemp -d -t ctemp.XXXXXXXXX)
function cleanup_temp () {
	rm -rf $SCRATCH
}
trap cleanup_temp EXIT

extra=
if [[ "$1" = "--scrub" ]]; then
    extra=--scrub
    shift 1
fi

THIS_DIR=$(cd $(dirname $0) && pwd);

RBMC_FLAGS="-A warnings -L $THIS_DIR/../lib"

IN_FILE="$1"

IN_NAME=$(basename "$IN_FILE")
MOD_NAME=${IN_NAME/.rs/}

echo "${MOD_NAME}_*" > $SCRATCH/api_filter
SRC_FILE=$SCRATCH/$IN_NAME
python $THIS_DIR/crust_macros.py "$1" > $SRC_FILE

$THIS_DIR/rbmc $RBMC_FLAGS "$SRC_FILE" >out.ir
cat out.ir $THIS_DIR/../stdlib.ir | $THIS_DIR/../src/Preprocess $extra >out2.ir
<out2.ir $THIS_DIR/crust.native -api-filter $SCRATCH/api_filter -optional-init - >out.c
#gcc -c out.c -std=c99 -I $THIS_DIR/../src
