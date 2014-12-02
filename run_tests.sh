#!/bin/bash

PROJECT_DIR=$(dirname $0);

make -C $PROJECT_DIR all > /dev/null

# find the rust libraries
RUSTC_LOC=$(which rustc)
if [ "$RUSTC_LOC" = "" ]; then
	echo "Could not find rustc";
	exit -1
fi

RUST_BIN=$(dirname $RUSTC_LOC)
RUST_LIB=$(find $RUST_BIN/../lib -mindepth 2 -maxdepth 2 -type d)/lib

RBMC_FLAGS="-A warnings -L $RUST_LIB"

for i in $(find $PROJECT_DIR/tests -type f -name '*.rs' | sort); do
	OUT_FILE=$(mktemp);
	$PROJECT_DIR/rbmc $RBMC_FLAGS $i 2> /dev/null | \
        $PROJECT_DIR/Preprocess 2> /dev/null | \
        $PROJECT_DIR/crust/crust.byte - 2> /dev/null > $OUT_FILE;
	if [ \! $? -eq 0 ]; then
		echo $(basename $i) FAILED;
		rm $OUT_FILE;
		continue;
	fi
	gcc -std=c99 -Wall -x c -c -o /dev/null $OUT_FILE;
	if [ $? -eq 0 ]; then
		echo $(basename $i) PASS
	else
		echo $(basename $i) FAILED
	fi
	rm $OUT_FILE;
done
