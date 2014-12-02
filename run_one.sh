#!/bin/bash
set -e

# find the rust libraries
RUSTC_LOC=$(which rustc)
if [ "$RUSTC_LOC" = "" ]; then
	echo "Could not find rustc";
	exit -1
fi

RUST_BIN=$(dirname $RUSTC_LOC)
RUST_LIB=$(find $RUST_BIN/../lib -mindepth 2 -maxdepth 2 -type d)/lib

RBMC_FLAGS="-A warnings -L $RUST_LIB"

./rbmc $RBMC_FLAGS "$1" >out.ir
<out.ir ./Preprocess >out2.ir
<out2.ir crust/crust.byte - >out.c
gcc -c out.c -std=c99
