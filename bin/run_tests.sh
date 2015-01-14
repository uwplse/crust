#!/bin/bash

BIN_DIR=$(dirname $0);
PROJECT_DIR=$BIN_DIR/../

make -C $PROJECT_DIR all > /dev/null

RBMC_FLAGS="-A warnings -L $PROJECT_DIR/lib"

for i in $(find $PROJECT_DIR/tests -type f -name '*.rs' | sort); do
	OUT_FILE=$(mktemp);
	$BIN_DIR/rbmc $RBMC_FLAGS $i 2> /dev/null | \
        $BIN_DIR/Preprocess 2> /dev/null | \
		$BIN_DIR/crust.native -gcc - 2> /dev/null > $OUT_FILE;
	if [ \! $? -eq 0 ]; then
		echo $(basename $i) FAILED;
		rm $OUT_FILE;
		continue;
	fi
	gcc -std=c99 -x c -c -o /dev/null $OUT_FILE;
	if [ $? -eq 0 ]; then
		echo $(basename $i) PASS
	else
		echo $(basename $i) FAILED
	fi
	rm $OUT_FILE;
done

