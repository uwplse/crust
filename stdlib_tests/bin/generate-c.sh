#!/bin/bash

input="$1"
base="${1%drv-fin.ir}"
output="${base}drv-split"

rm -f "${output}"-*.ir
python "$(dirname "$0")/split-driver-fns.py" "$input" "$output"

for f in "${output}"-*.ir; do
    i=${f##*-}
    i=${i%.ir}
    rm -f "${base}${i}.c"
    echo $i
done | parallel "$(dirname "$0")/../../bin/crust.native ${output}-{}.ir >${base}{}.c" 

