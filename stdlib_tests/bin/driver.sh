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

trans_bin() {
    edo ../bin/rbmc $rustc_args $1 >ir/aout.ir
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
	cat ir/lib*.ir | ../bin/Preprocess --scrub > ./ir/stdlib.ir
}

"$@"
