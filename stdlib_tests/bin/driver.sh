#!/bin/bash
set -e
set -u

if ! [[ -e src ]]; then
    echo "You must create a symlink ./src pointing to the rust source code"
    exit 1
fi

mkdir -p lib ir


edo() {
    echo $'\x1b[32m ===' "$@" $'\x1b[0m' 1>&2
    "$@"
}

rustc_args="-L lib --out-dir=lib --target=x86_64-custom-linux-gnu.json"
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

get_test_num() {
	t_num=$(basename $1)
	t_num=${t_num/.rs/}
	t_num=${t_num/libtest_/}
	echo $t_num
}

scrub_test_error() {
	TO_SCRUB=$1
	test_num=$(get_test_num $TO_SCRUB)
	for i in $(seq 0 3); do
		if run_rbmc $TO_SCRUB > $SCRATCH/test_${test_num}.ir 2> $SCRATCH/failing_tests_${test_num}; then
			rm -f $SCRATCH/scrubbed_*_${test_num}.rs $SCRATCH/failing_tests_${test_num} $SCRATCH/pre_scrubbed_*_${test_num}.rs
			return
		fi
		cp $TO_SCRUB $SCRATCH/pre_scrubbed_${i}_${test_num}.rs
		egrep -o '.+error:' $SCRATCH/failing_tests_${test_num} | sed -r -e 's/[^:]+:([[:digit:]]+):.+/\1/g' | python ../bin/filter_errors.py $TO_SCRUB > $SCRATCH/scrubbed_${i}_${test_num}.rs;
		cp $SCRATCH/scrubbed_${i}_${test_num}.rs $TO_SCRUB
	done
	echo "Failed to scrub tests"
	exit 1
}

compile_test () {
	scrub_test_error $1;
	test_num=$(get_test_num $1)
	../bin/Preprocess > $SCRATCH/test2_${test_num}.ir < $SCRATCH/test_${test_num}.ir;
	cat $2 >> $SCRATCH/test2_${test_num}.ir;
	../bin/crust.native -test-compile $SCRATCH/test2_${test_num}.ir;
	rm $SCRATCH/test_${test_num}.ir $SCRATCH/test2_${test_num}.ir
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
		rm patches/last.patch
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
	test_num=$(get_test_num $1)
	python ../bin/crust_macros.py --intrinsics $1 > $SCRATCH/temp_${test_num}.rs
	mv $SCRATCH/temp_${test_num}.rs $1
}

dump_items() {
	../bin/crust.native -dump-items -api-filter $1 ir/stdlib.ir
}

dump_api() {
	../bin/crust.native -dump-heuristic -api-filter $1 ir/stdlib.ir
}

trans_test() {
	instrument_intrinsics $1
	OUTPUT_NAME=$(basename $1);
	OUTPUT_NAME=${OUTPUT_NAME/.rs/.c}
	edo compile_test $1 $SCRATCH/simple_ir > $2/$OUTPUT_NAME
}

DRIVER_FLAGS="-driver-gen -immut-length 2 -mut-length 3"

trans_stdlib_test() {
	local filter=$1
	local dir=$2
	shift 2
	if [ \! -e $dir ]; then
		mkdir -p $dir
	fi
	dump_items $filter > $SCRATCH/item_filter
	../bin/Preprocess --filter $SCRATCH/item_filter < ./ir/stdlib.ir > $SCRATCH/simple_ir
	mkdir -p $SCRATCH/test_cases
	edo ../bin/crust.native $DRIVER_FLAGS -api-filter $filter "$@" -test-case-prefix $SCRATCH/test_cases/libtest $SCRATCH/simple_ir
	ls $SCRATCH/test_cases/*.rs | parallel --halt 2 bash $0 set_scratch $SCRATCH trans_test '{}' $dir
}

_prepare_remote_build() {
	filter=$1
	output_file=$2
	shift 2
	dump_items $filter > $SCRATCH/item_filter
	../bin/Preprocess --filter $SCRATCH/item_filter < ./ir/stdlib.ir > $SCRATCH/simple_ir
	mkdir -p $SCRATCH/test_cases
	edo ../bin/crust.native $DRIVER_FLAGS -api-filter $filter "$@" -test-case-prefix $SCRATCH/test_cases/libtest $SCRATCH/simple_ir
	tar cf $output_file -C $SCRATCH test_cases simple_ir
}

prepare_remote_build() {
	local patch=$1
	local lib_files=$2
	shift 2
	apply_and_build $patch
	with_scratch _prepare_remote_build "$@"
	tar cf $lib_files -C ./lib $(cd lib; ls *.rlib)
}

_do_remote_build() {
	rm -f ~/comp_tests.tar.bz2
	rm -rf ~/built_tests
	mkdir -p ~/built_tests 2> /dev/null
	tar xf $1 -C lib
	tar xf $2 -C $SCRATCH
	ls $SCRATCH/test_cases/*.rs | parallel --halt 2 bash $0 set_scratch $SCRATCH trans_test '{}' ~/built_tests
	tar cf ~/comp_tests.tar.bz2 -C ~/built_tests $(cd ~/built_tests; ls *.c)
}

do_remote_build() {
	with_scratch _do_remote_build $1 $2
}

apply_and_build() {
	PATCH_SUM=$(md5sum $1 | awk '{print $1}');
	apply_patch $1
	if [ -e "precomp/${PATCH_SUM}.ir" ]; then
		cp "precomp/${PATCH_SUM}.ir" ./ir/stdlib.ir
	else
		trans_stdlib
		cp ir/stdlib.ir "precomp/${PATCH_SUM}.ir"
	fi
	build_all_libs
}

# [patch] [filter] [output]
build_test() {
	apply_and_build $1
	with_scratch trans_stdlib_test $2 $3
}

prepare_remote_ir() {
	PATCH_SUM=$(md5sum $1 | awk '{print $1}')
	if [ -e "$2/${PATCH_SUM}.tar.bz2" ]; then
		exit 0
	fi
	apply_patch $1;
	rm -f ir/stdlib.ir
	trans_all_libs
	tar cvf $2/${PATCH_SUM}.tar.bz2 -j -C ir $(cd ir; ls *.ir)
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

build_cache() {
	for i in "$@"; do
		checksum=$(md5sum $i | awk '{print $1}');
		apply_patch $i;
		trans_stdlib
		cp ./ir/stdlib.ir ./precomp/${checksum}.ir
	done
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

evaluate_heuristics() {
	local args
	for h in default symm-break interfere-check interest-filter mut-analysis copy-check; do
		if [[ "$h" == "default" ]]; then
			args=
		else
			args=-no-$h
		fi
		mkdir -p hcount/$h hcount/${h}_scratch
		edo tmux new-window "bash $0 set_scratch hcount/${h}_scratch trans_stdlib_test vec.filter hcount/$h $args |& tee hcount/${h}.log"
	done
}

"$@"
