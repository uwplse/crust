#!/bin/bash

RBMC_DIR=$(cd $(dirname $0) && pwd);

# find the rust libraries
RUSTC_LOC=$(which rustc)
if [ "$RUSTC_LOC" = "" ]; then
	echo "Could not find rustc";
	exit -1
fi

RUST_BIN=$(dirname $RUSTC_LOC)
RUST_LIB=$(find $RUST_BIN/../lib -mindepth 2 -maxdepth 2 -type d)/lib

RBMC_FLAGS="-A warnings -L $RUST_LIB"

$RBMC_DIR/rbmc $RBMC_FLAGS "$1"
