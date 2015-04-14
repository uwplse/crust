#!/bin/bash
set -e

exec 3>&2

edo() { echo "$@" 1>&3; "$@"; }

#edo rm lib/* ir/*
#edo bin/driver.mk ir/stdlibs.ir
edo ../bin/Preprocess --passes hl-generate-drivers --merged-filter filters/vecnull.filter <ir/stdlibs.ir >vecnull.drv0 2>log-gen.txt
edo ../bin/Preprocess --passes hl-clean-drivers <vecnull.drv0 >vecnull.drv 2>log-clean.txt
edo ../bin/crust.native -driver-gen <vecnull.drv
edo ../bin/rbmc -L lib _0.rs --target=x86_64-custom-linux-gnu.json >_0.ir
cat _0.ir vecnull.drv0 | edo ../bin/Preprocess --passes hl-compile-drivers >vecnull_tests.ir 2>log-compile.txt
edo ../bin/crust.native -gcc vecnull_tests.ir >vecnull_tests.c
