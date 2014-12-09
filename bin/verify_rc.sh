#!/bin/bash

TEMP_DIR=$(mktemp -d);

function cleanup {
	rm -rf $TEMP_DIR
}

trap cleanup EXIT

BIN_DIR=$(cd $(dirname $0) && pwd);
ROOT_DIR=$BIN_DIR/../

function run_toolchain {
	rm -f $TEMP_DIR/*
	cp $ROOT_DIR/tests/refcell.rs $TEMP_DIR;
	if [ $# -eq 1 ]; then
		patch -d $TEMP_DIR -p2 < $1
	fi
	bash $BIN_DIR/run_rbmc.sh $TEMP_DIR/refcell.rs | $BIN_DIR/Preprocess | $BIN_DIR/crust.native - > $TEMP_DIR/refcell.c
	cbmc $TEMP_DIR/refcell.c 2> /dev/null > /dev/null
}

echo "Verifying initial state"
run_toolchain;
if [ $? -ne 0 ]; then
 	echo "Base version failed to verify"
	exit -1
fi

for patch in $(find $ROOT_DIR/tests -name '*.patch'); do
	run_toolchain $patch
	if [ $? -eq 0 ]; then
		echo "Verification succeeded, when failure was expected with patch $patch"
		exit -1
	fi
done
