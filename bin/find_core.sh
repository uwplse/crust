#!/bin/bash

set -e

ROOT_DIR=$(cd $(dirname $0) && pwd);

# find the rust libraries
RUSTC_LOC=$(which rustc)
if [ "$RUSTC_LOC" = "" ]; then
	echo "Could not find rustc";
	exit -1
fi

RUST_BIN=$(dirname $RUSTC_LOC)
RUST_LIB=$(find $RUST_BIN/../lib -mindepth 1 -type d -name lib)

ln -s $RUST_LIB $ROOT_DIR/../lib
