#!/bin/bash
set -e

THIS_DIR=$(cd $(dirname $0) && pwd);

RBMC_FLAGS="-A warnings -L $THIS_DIR/../lib"

$THIS_DIR/../src/rbmc $RBMC_FLAGS "$1" >out.ir
<out.ir $THIS_DIR/../src/Preprocess >out2.ir
<out2.ir $THIS_DIR/../src/crust/crust.native -gcc - >out.c
gcc -c out.c -std=c99 -I $THIS_DIR/../src
