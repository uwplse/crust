#!/bin/bash

THIS_DIR=$(cd $(dirname $0) && pwd);

SCRATCH=$(mktemp -d /tmp/crust.XXXXXXX)
function cleanup() {
	rm -rf $SCRATCH;
}
trap cleanup EXIT;

INPUT_FILE=$1

INPUT_MODULE=$(basename $INPUT_FILE)
INPUT_MODULE=${INPUT_MODULE/.rs/}

python $THIS_DIR/crust_macros.py $INPUT_FILE  > $SCRATCH/${INPUT_MODULE}.rs
$THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $SCRATCH/${INPUT_MODULE}.rs > $SCRATCH/module.ir
cat $SCRATCH/module.ir $THIS_DIR/../stdlib.ir | $THIS_DIR/Preprocess 2> /dev/null > $SCRATCH/pp_module.ir

$THIS_DIR/crust.native -driver-gen -set-api-filter "${INPUT_MODULE}\$*" $SCRATCH/pp_module.ir > $SCRATCH/${INPUT_MODULE}_tests_temp.rs;
python $THIS_DIR/crust_macros.py --intrinsics --module $INPUT_MODULE $SCRATCH/${INPUT_MODULE}_tests_temp.rs > $SCRATCH/${INPUT_MODULE}_tests.rs

cp $INPUT_FILE $SCRATCH
$THIS_DIR/rbmc -A warnings -L $THIS_DIR/../lib $SCRATCH/${INPUT_MODULE}_tests.rs > $SCRATCH/${INPUT_MODULE}_tests.ir
cat $SCRATCH/pp_module.ir $SCRATCH/${INPUT_MODULE}_tests.ir | $THIS_DIR/Preprocess 2> /dev/null > $SCRATCH/${INPUT_MODULE}_tests_pp.ir
$THIS_DIR/crust.native -test-compile $SCRATCH/${INPUT_MODULE}_tests_pp.ir > ${INPUT_MODULE}_tests.c
