#!/bin/bash

cfile=$1
[[ -n "$cfile" ]] || { echo "must provide an input C file"; exit 1; }

for f in "${cfile}".*.c; do
    grep '^rs_unit [^ ]*\$__crust_test_[0-9]*();$' "$f" | \
        sed -e 's/^rs_unit //' -e 's/();$//' -e "s:^:$f/:"
done | \
    parallel \
        --results "results/$(date +%Y%m%d-%H%M%S)-$(basename "$cfile")" \
        --timeout 300 \
        cbmc --unwind 5 --no-unwinding-assertions -I ../src --ILP32 --function '{/}' '{//}'
