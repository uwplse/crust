#!/bin/bash
set -e

THIS_DIR=$(cd $(dirname $0) && pwd);

the_lib=$1

if [ -z "$THE_LIB_SRC" ]; then
	# find the library sources
	RUST_LOC=$(which rustc)
	if [ "$RUST_LOC" = "" ]; then
		echo "Could not find rustc";
		exit -1
	fi
	while true; do
		THE_LIB_SRC="$RUST_LOC/src/$the_lib/lib.rs"
		if [[ -f "$THE_LIB_SRC" ]]; then
			break
		elif [[ "$RUST_LOC" = "/" ]]; then
			echo "could not find $the_lib sources"
			exit -1
		else
			RUST_LOC=$(dirname "$RUST_LOC")
		fi
	done
fi

RBMC_FLAGS="-A warnings -L $THIS_DIR/../lib"

echo "running rbmc"
if [[ "$THIS_DIR/rbmc" -nt "$the_lib.ir" ]]; then
    $THIS_DIR/rbmc $RBMC_FLAGS "$THE_LIB_SRC" >${the_lib}.ir || true
fi
echo "running Preprocess"
<${the_lib}.ir sed -e '/^#/s/.*//' | $THIS_DIR/../src/Preprocess --scrub >${the_lib}2.ir
echo "running crust.native"
<${the_lib}2.ir $THIS_DIR/../src/crust/crust.native -api-filter $THIS_DIR/../src/crust/core_filter -optional-init -gcc - >${the_lib}.c
echo "running gcc"
gcc -c ${the_lib}.c -std=c99 -I $THIS_DIR/../src
