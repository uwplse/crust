#!/bin/bash

set -e
set -u

cd ~/

declare -a WAITING_JOBS
WAITING_JOBS=()

rm -rf ~/workdir
mkdir -p ~/workdir 2> /dev/null

rm -rf ~/built_ir/
mkdir -p ~/built_ir/ 2> /dev/null

for i in *.tar.bz2; do
	PATCH_SUM=$(basename $i)
	PATCH_SUM=${PATCH_SUM/.tar.bz2/}
	mkdir -p ~/workdir/$PATCH_SUM 2> /dev/null
	tar xf $i -C ~/workdir/$PATCH_SUM;
	cat ~/workdir/$PATCH_SUM/*.ir | ~/bin/Preprocess --scrub 2> /dev/null > ~/built_ir/${PATCH_SUM}.ir &
	WAITING_JOBS[${#WAITING_JOBS[@]}]=$!
done

for pid in ${WAITING_JOBS[@]}; do
	wait $pid
done

(cd ~/built_ir; tar cf ~/comp_ir.tar.bz2 *.ir)
