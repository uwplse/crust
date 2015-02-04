#!/bin/bash
set -e

extra=
if [[ "$1" = "--scrub" ]]; then
    extra=--scrub
    shift 1
fi

THIS_DIR=$(cd $(dirname $0) && pwd);

RBMC_FLAGS="-A warnings -L $THIS_DIR/../lib"

$THIS_DIR/rbmc $RBMC_FLAGS "$1" >out.ir
cat out.ir stdlib.ir | $THIS_DIR/../src/Preprocess $extra >out2.ir
<out2.ir $THIS_DIR/../src/crust/crust.native -api-filter $THIS_DIR/../src/crust/core_filter -optional-init - >out.c
#gcc -c out.c -std=c99 -I $THIS_DIR/../src
