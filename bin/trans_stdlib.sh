#!/bin/bash
set -e

THIS_DIR=$(cd $(dirname $0) && pwd);

# find the library sources
if [[ -z "$RUST_SRC" ]]; then
    RUST_LOC=$(which rustc)
    if [ "$RUST_LOC" = "" ]; then
        echo "Could not find rustc";
        exit -1
    fi
    while true; do
        RUST_SRC="$RUST_LOC/src"
        if [[ -d "$RUST_SRC" ]]; then
            break
        elif [[ "$RUST_LOC" = "/" ]]; then
            echo "could not find rust sources"
            exit -1
        else
            RUST_LOC=$(dirname "$RUST_LOC")
        fi
    done
fi

inputs=

for libname in core libc alloc; do
    the_lib=lib${libname}

    RBMC_FLAGS="-A warnings -L $THIS_DIR/../lib"

    echo $'\x1b[34m'"running rbmc on $the_lib"$'\x1b[0m'
    if [[ "$THIS_DIR/rbmc" -nt "${the_lib}.ir" ]]; then
        $THIS_DIR/rbmc $RBMC_FLAGS "$RUST_SRC/${the_lib}/lib.rs" >${the_lib}.ir || true
    fi
    inputs="$inputs ${the_lib}.ir"
done

echo "running Preprocess"
cat $inputs | $THIS_DIR/../src/Preprocess --scrub >stdlib.ir
