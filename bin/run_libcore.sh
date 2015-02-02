#!/bin/bash
set -e

THIS_DIR=$(cd $(dirname $0) && pwd);

if [ -z "$LIBCORE_SRC" ]; then
	# find the libcore sources
	RUST_LOC=$(which rustc)
	if [ "$RUST_LOC" = "" ]; then
		echo "Could not find rustc";
		exit -1
	fi
	while true; do
		LIBCORE_SRC="$RUST_LOC/src/libcore/lib.rs"
		if [[ -f "$LIBCORE_SRC" ]]; then
			break
		elif [[ "$RUST_LOC" = "/" ]]; then
			echo "could not find libcore sources"
			exit -1
		else
			RUST_LOC=$(dirname "$RUST_LOC")
		fi
	done
fi

RBMC_FLAGS="-A warnings -L $THIS_DIR/../lib"

echo "running rbmc"
if [[ "$THIS_DIR/rbmc" -nt "libcore.ir" ]]; then
    $THIS_DIR/rbmc $RBMC_FLAGS "$LIBCORE_SRC" >libcore.ir || true
fi
echo "running Preprocess"
<libcore.ir sed -e '/^#/s/.*//' | $THIS_DIR/../src/Preprocess --scrub >libcore2.ir
echo "running crust.native"
<libcore2.ir $THIS_DIR/../src/crust/crust.native -api-filter $THIS_DIR/../src/crust/core_filter -optional-init -gcc - >libcore.c
echo "running gcc"
gcc -c libcore.c -std=c99 -I $THIS_DIR/../src
