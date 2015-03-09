#!/bin/bash
set -e

if ! [[ -e src ]]; then
    echo "You must create a symlink ./src pointing to the rust source code"
    exit 1
fi

mkdir -p lib ir


edo() {
    echo $'\x1b[32m ===' "$@" $'\x1b[0m' 1>&2
    "$@"
}

rustc_args="-L lib --out-dir=lib --target=x86_64-custom-linux-gnu.json -A warnings"
stdlibs="core libc alloc unicode collections"


SCRATCH=.

copy_compiler_rt() {
    RUST_HOME=$(dirname $(dirname $(which rustc)))
    cp -v $RUST_HOME/lib/rustlib/*/lib/libcompiler-rt.a lib
}


build_lib() {
    edo rustc $rustc_args $1
}

build_std_lib() {
    build_lib src/lib${1}/lib.rs
}

build_all_libs() {
    for lib in $stdlibs; do
        build_std_lib $lib
    done
    build_lib simplert.rs
}

build_bin() {
    edo rustc $rustc_args $1 --crate-type=staticlib -o lib/libaout.a
    edo gcc lib/libaout.a -lm
}

build_and_check() {
    build_bin $1
    edo ./a.out; echo Result: $?
}


trans_std_lib() {
    edo ../bin/rbmc $rustc_args src/lib${1}/lib.rs >ir/lib${1}.ir
}

trans_all_libs() {
    lib_irs=
    for lib in $stdlibs; do
        trans_std_lib $lib
        lib_irs="ir/lib${lib}.ir $lib_irs"
    done
}

scrub_test_error() {
	TO_SCRUB=$1
	for i in $(seq 0 3); do
		if run_rbmc $TO_SCRUB > $SCRATCH/tests.ir 2> $SCRATCH/failing_tests; then
			return
		fi
		cp $TO_SCRUB $SCRATCH/pre_scrubbed_${i}.rs
		egrep -o '.+error:' $SCRATCH/failing_tests | sed -r -e 's/[^:]+:([[:digit:]]+):.+/\1/g' | python ../bin/filter_errors.py $TO_SCRUB > $SCRATCH/scrubbed_${i}.rs;
		cp $SCRATCH/scrubbed_${i}.rs $TO_SCRUB
	done
	echo "Failed to scrub tests"
	exit 1
}

compile_test () {
	scrub_test_error $1;
	../bin/Preprocess > $SCRATCH/tests2.ir < $SCRATCH/tests.ir;
	cat $2 >> $SCRATCH/tests2.ir;
	../bin/crust.native -test-compile $SCRATCH/tests2.ir;
}

run_rbmc() {
	../bin/rbmc $rustc_args $1
}

trans_bin() {
    edo run_rbmc $1 >ir/aout.ir
}


apply_patch() {
    unapply_last_patch
    patch -p1 <"$1"
    rm -f patches/last.patch
    ln -s $(readlink -f "$1") patches/last.patch
}

unapply_last_patch() {
    if [[ -e "patches/last.patch" ]]; then
        patch -p1 -R <patches/last.patch
    fi
}

trans_stdlib() {
	trans_all_libs
	cat ir/lib*.ir | ../bin/Preprocess --scrub > ./ir/stdlib.ir 2> ./pp_out
}

scratch_cleanup() {
	rm -rf $SCRATCH
}

instrument_intrinsics() {
	python ../bin/crust_macros.py --intrinsics $1 > $SCRATCH/temp.rs
	mv $SCRATCH/temp.rs $1
}

dump_items() {
	../bin/crust.native -dump-items -api-filter $1 ir/stdlib.ir
}

dump_api() {
	 ../bin/crust.native -dump-api -api-filter $1 ir/stdlib.ir
}

trans_test() {
	instrument_intrinsics $1
	OUTPUT_NAME=$(basename $1);
	OUTPUT_NAME=${OUTPUT_NAME/.rs/.c}
	compile_test $1 $SCRATCH/simple_ir > $2/$OUTPUT_NAME
}

trans_stdlib_test() {
	if [ \! -e $2 ]; then
		mkdir -p $2
	fi
	dump_items $1 > $SCRATCH/item_filter
	../bin/Preprocess --filter $SCRATCH/item_filter < ./ir/stdlib.ir > $SCRATCH/simple_ir
	mkdir -p $SCRATCH/test_cases
	../bin/crust.native -driver-gen -api-filter $1 -no-mut-analysis -immut-length 0 -mut-length 4 -test-case-prefix $SCRATCH/test_cases/libtest $SCRATCH/simple_ir
	for i in $SCRATCH/test_cases/*.rs; do
		trans_test $i $2;
	done
}

with_scratch() {
	if [ "$SCRATCH" = "." ] ; then
		SCRATCH=$(mktemp -d /tmp/crust.XXXXXXX)
		#trap scratch_cleanup EXIT;
		"$@"
	else
		echo "Scratch already set!";
		exit -1;
	fi
}

set_scratch() {
	if [ "$SCRATCH" = "." ]; then
		SCRATCH=$1
		shift
		"$@"
	else
		echo "Scratch already set!";
		exit -1
	fi
}

"$@"
