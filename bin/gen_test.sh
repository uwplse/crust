#!/bin/bash

set -x
set -e

THIS_DIR=$(cd $(dirname $0) && pwd);

GEN_ARGS=

while [ $# -gt 1 ]; do
	GEN_ARGS="$GEN_ARGS $1";
	shift 1
done

SCRATCH=$(mktemp -d /tmp/crust.XXXXXXX)
function cleanup() {
	rm -rf $SCRATCH;
}
trap cleanup EXIT;

INPUT_FILE=$1

INPUT_MODULE=$(basename $INPUT_FILE)
INPUT_MODULE=${INPUT_MODULE/.rs/}

OUTPUT_FOLDER=${INPUT_MODULE}_tests

if [ -e $OUTPUT_FOLDER ]; then
	echo "Output location $OUTPUT_FOLDER already exists; refusing to run"
	exit 1
fi

function do_scrub() {
	TO_SCRUB=$1
	for i in $(seq 0 2); do
		if $THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $TO_SCRUB > $SCRATCH/tests.ir 2> $SCRATCH/failing_tests; then
			return
		fi
		cp $TO_SCRUB $SCRATCH/pre_scrubbed_${i}.rs
		egrep -o '.+error:' $SCRATCH/failing_tests | sed -r -e 's/[^:]+:([[:digit:]]+):.+/\1/g' | python $THIS_DIR/filter_errors.py $TO_SCRUB > $SCRATCH/scrubbed_${i}.rs;		
		cp $SCRATCH/scrubbed_${i}.rs $TO_SCRUB
	done
	echo "Failed to scrub tests"
	exit 1
}

TEST_FILES=$SCRATCH/tests
TRANS_FILES=$SCRATCH/$OUTPUT_FOLDER

mkdir $TEST_FILES $TRANS_FILES

cp $INPUT_FILE $TEST_FILES

function trans_test() {
	TO_TRANS=$1;
	shift 1;
	OUT_NAME=$(basename $TO_TRANS)
	OUT_PATH=$TRANS_FILES/$OUT_NAME
	OUT_PATH=${OUT_PATH/.rs/.c}

	python $THIS_DIR/crust_macros.py --intrinsics --module $INPUT_MODULE $TO_TRANS > $SCRATCH/temp.rs
	mv $SCRATCH/temp.rs $TO_TRANS

	do_scrub $TO_TRANS
	
	cat $SCRATCH/pp_module.ir $SCRATCH/tests.ir | $THIS_DIR/Preprocess 2> /dev/null > $SCRATCH/pp_test.ir

	$THIS_DIR/crust.native -test-compile $SCRATCH/pp_test.ir > $OUT_PATH
}

python $THIS_DIR/crust_macros.py $INPUT_FILE > $SCRATCH/${INPUT_MODULE}.rs
$THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $SCRATCH/${INPUT_MODULE}.rs > $SCRATCH/module.ir
cat $SCRATCH/module.ir $THIS_DIR/../stdlib.ir | $THIS_DIR/Preprocess 2> /dev/null > $SCRATCH/pp_module.ir
$THIS_DIR/crust.native $GEN_ARGS -driver-gen -set-api-filter "${INPUT_MODULE}\$*" -test-case-prefix $TEST_FILES/${INPUT_MODULE}_test $SCRATCH/pp_module.ir
for i in $TEST_FILES/${INPUT_MODULE}_test_*.rs; do
	trans_test $i
done


mv $TRANS_FILES .
