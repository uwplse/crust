#!/bin/bash
set -e

name=$1
variant=$2

dir=evaluation-$variant/$name-$(date +%Y%m%d-%H%M%S)
mkdir -p $dir

command time bin/driver.mk ir/stdlibs.stubs.ir |& tee $dir/00-stdlibs.log

rm -f driver/* test/*
command time bin/driver.mk test/${name}.drv-fin.ir |& tee $dir/01-gen1.log
command time bin/generate-c.sh test/${name}.drv-fin.ir |& tee $dir/02-gen2.log
command time bin/run_tests.sh test/${name} |& tee $dir/03-check.log

mkdir $dir/driver $dir/test
cp -r ir lib $dir/
mv driver/* $dir/driver
mv test/* $dir/test

results=$(ls -d results/*-${name} --sort=time | head -n 1)
mv $results $dir/results

command time bin/collect_failing.sh $dir/results $dir/test/${name}.rs |& tee $dir/04-results.log

