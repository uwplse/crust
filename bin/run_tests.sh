#!/bin/bash
set -o pipefail

BIN_DIR=$(dirname $0);
PROJECT_DIR=$BIN_DIR/../

make -C $PROJECT_DIR all > /dev/null

RBMC_FLAGS="-A warnings -L $PROJECT_DIR/lib"

FAILED=$'\x1b[20G\x1b[31mFAILED\x1b[0m'
RBMC_PANIC=$'\x1b[20G\x1b[33mRBMC PANIC\x1b[0m'
PASSED=$'\x1b[20G\x1b[32mPASSED\x1b[0m'

for i in $(find $PROJECT_DIR/tests -type f -name '*.rs' | sort); do
	OUT_FILE=$(mktemp);
	$BIN_DIR/rbmc $RBMC_FLAGS $i 2>rbmc.log | \
        $BIN_DIR/Preprocess 2> /dev/null | \
		$BIN_DIR/crust.native -optional-init -gcc - 2> /dev/null > $OUT_FILE;
	if [ \! $? -eq 0 ]; then
		echo $(basename $i) $FAILED;
		rm $OUT_FILE;
		continue;
	fi
	gcc -std=c99 -x c -I $PROJECT_DIR/src -c -o /dev/null $OUT_FILE;
    ret=$?
    if grep panic rbmc.log >/dev/null; then
        echo $(basename $i) $RBMC_PANIC
	elif [ $ret -eq 0 ]; then
		echo $(basename $i) $PASSED
	else
		echo $(basename $i) $FAILED
	fi
	rm $OUT_FILE;
done

