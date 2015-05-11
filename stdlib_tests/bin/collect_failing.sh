#!/bin/bash

dir=$1
if [[ -z "$dir" ]]; then
    dir=$(ls -d results/* --sort=time | head -n 1)
fi
if ! [[ -d "$dir" ]]; then
    echo "not a directory: $dir"
    exit 1
fi

echo "collecting failed results from ${dir}"

input_file="${dir%/*/*/*}"
input_file="$(basename "$input_file")"
input_file="${input_file#*-*-}"
input_file="test/${input_file}.rs"
echo "input file: $input_file"

num_failed=0
for f in "$dir/1/"*"/stdout"; do
    msg="$(tail -n 1 "$f")"
    if [[ "$msg" != "VERIFICATION SUCCESSFUL" ]]; then
        fn="$(basename "$(dirname "$f")")"
        fn="${fn##*$}"
        echo "FAILED: $fn"

        awk '/Violated property:/ {go=1} /^$/ {go=0} {if (go) print}' "$f"
        echo

        awk '/^fn/ {go=0} /^fn '$fn'\(/ {go=1} {if (go) print}' "$input_file" | \
            python "$(dirname "$0")/simple_format.py"
        echo

        num_failed=$((num_failed + 1))
    fi
done

echo "$num_failed tests failed"
