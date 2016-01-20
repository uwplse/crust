#!/bin/bash

cfile=$1
[[ -n "$cfile" ]] || { echo "must provide an input C file"; exit 1; }

grep '^rs_unit [^ ]*\$__crust_test_[0-9]*();$' "$cfile" | \
    sed -e 's/^rs_unit //' -e 's/();$//' -e "s:^:$cfile/:" | \
    parallel \
        --results "results/$(date +%Y%m%d-%H%M%S)-$(basename "$cfile" .c)" \
        --timeout 300 \
        cbmc --unwind 5 --no-unwinding-assertions -I ../src --ILP32 \
            --pointer-check --bounds-check \
            --function '{/}' '{//}'

# No --z3 because that backend can't handle empty structs like rs_unit.
