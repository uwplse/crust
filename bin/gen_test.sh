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

python $THIS_DIR/crust_macros.py $INPUT_FILE  > $SCRATCH/${INPUT_MODULE}.rs
$THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $SCRATCH/${INPUT_MODULE}.rs > $SCRATCH/module.ir
cat $SCRATCH/module.ir $THIS_DIR/../stdlib.ir | $THIS_DIR/Preprocess 2> /dev/null > $SCRATCH/pp_module.ir

$THIS_DIR/crust.native $GEN_ARGS -driver-gen -set-api-filter "${INPUT_MODULE}\$*" $SCRATCH/pp_module.ir > $SCRATCH/${INPUT_MODULE}_tests_temp.rs;
python $THIS_DIR/crust_macros.py --intrinsics --module $INPUT_MODULE $SCRATCH/${INPUT_MODULE}_tests_temp.rs > $SCRATCH/${INPUT_MODULE}_tests.rs

cp $INPUT_FILE $SCRATCH
if $THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $SCRATCH/${INPUT_MODULE}_tests.rs > $SCRATCH/${INPUT_MODULE}_tests.ir 2> $SCRATCH/failing_tests; then
	true
else
	cp $SCRATCH/${INPUT_MODULE}_tests.rs $SCRATCH/pre_scrubbed.rs
	python $THIS_DIR/filter_errors.py $SCRATCH/${INPUT_MODULE}_tests.rs $(egrep -o '.+error:' $SCRATCH/failing_tests | sed -r -e 's/[^:]+:([[:digit:]]+):.+/\1/g' | paste -s -d',') > $SCRATCH/scrubbed.rs;

	cp $SCRATCH/scrubbed.rs $SCRATCH/${INPUT_MODULE}_tests.rs;

	$THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $SCRATCH/${INPUT_MODULE}_tests.rs > $SCRATCH/${INPUT_MODULE}_tests.ir
fi
cat $SCRATCH/pp_module.ir $SCRATCH/${INPUT_MODULE}_tests.ir | $THIS_DIR/Preprocess 2> /dev/null > $SCRATCH/${INPUT_MODULE}_tests_pp.ir
mkdir $OUTPUT_FOLDER
$THIS_DIR/crust.native -test-compile -test-case-prefix "${OUTPUT_FOLDER}/${INPUT_MODULE}_test" $SCRATCH/${INPUT_MODULE}_tests_pp.ir
