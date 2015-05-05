#!/bin/bash

cfile=$1
[[ -n "$cfile" ]] || { echo "must provide an input C file"; exit 1; }

grep '^rs_unit [^ ]*\$__crust_test_[0-9]*();$' "$cfile" | \
    sed -e 's/^rs_unit //' -e 's/();$//' | \
    parallel --results "results/$(date +%Y%m%d-%H%M%S)-$(basename "$cfile" .c)" \
        cbmc --unwind 5 --no-unwinding-assertions -I ../src --ILP32 --function '{}' "$cfile"
