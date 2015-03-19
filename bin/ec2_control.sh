#!/bin/bash

set -e
set -u

THIS_DIR=$(cd $(dirname $0) && pwd)

. $THIS_DIR/util.sh

function mkworker() {
	ROOT_DIR=$(cd $THIS_DIR/../ && pwd)
	TEST_DIR=$(cd $1 && pwd)
	TEST_DIR=${TEST_DIR#$ROOT_DIR/}
	tar cvf /tmp/crust_worker.tar.bz2 -j -C $THIS_DIR/../  src/rust_intrinsics.h src/crust_intrinsics.h bin/run_tests.py $TEST_DIR
	
	mv /tmp/crust_worker.tar.bz2 .
}

function worker_scp() {
	scp -i $THIS_DIR/crust_test.pem $2 ec2-user@$1:$3
}

function build_worker() {
	z3_loc=$(which z3);
	host=$1
	ssh -i $THIS_DIR/crust_test.pem ec2-user@$host rm -rf ~/cbmc-5.0 ~/crust ~/z3;
	worker_scp $host $THIS_DIR/../cbmc-5.0.tar.bz2 "~/cbmc-5.0.tar.bz2"
	worker_scp $host $z3_loc "~/z3"
	worker_scp $host $THIS_DIR/setup_worker.sh "~/setup_worker.sh"
	worker_scp $host $2 "~/crust_worker.tar.bz2"
	ssh -t -i $THIS_DIR/crust_test.pem ec2-user@$host "bash /home/ec2-user/setup_worker.sh; exit"
}

function deploy_worker() {
	worker_scp $1 $2 "~/crust_worker.tar.bz2"
	ssh -i $THIS_DIR/crust_test.pem ec2-user@$1 "rm -rf ~/crust/*"
	ssh -i $THIS_DIR/crust_test.pem ec2-user@$1 "tar xvf ~/crust_worker.tar.bz2 -C ~/crust"
}

function deploy_all_workers() {
    for host in ${WORKER_IPS[@]}; do
		deploy_worker $host $1
	done
}

function get_results() {
	ssh -i $THIS_DIR/crust_test.pem ubuntu@$JOBHOST "tar cf ~/_temp_results.tar.bz2 -C ~/workqueue/ results";
	scp -i $THIS_DIR/crust_test.pem ubuntu@$JOBHOST:~/_temp_results.tar.bz2 $1
	ssh -i $THIS_DIR/crust_test.pem ubuntu@$JOBHOST "rm ~/_temp_results.tar.bz2"
}

function build_all_workers() {
	for host in ${WORKER_IPS[@]}; do
		build_worker $host $1
	done
}

function launch_worker() {
	ssh -i $THIS_DIR/crust_test.pem ec2-user@$1 'cd /home/ec2-user/crust/stdlib_tests/vecu8_tests; TMPDIR=/media/ephemeral0/smt_temp/ nohup python2.7 ../../bin/run_tests.py --cbmc=/home/ec2-user/cbmc-5.0/src/cbmc/cbmc --timeout=3600 --unwind=5 --job-host="http://'"$JOBHOST"':5000" --worker > ~/crust.out 2> ~/crust.err < /dev/null &'
}

function launch_all_workers() {
	for host in ${WORKER_IPS[@]}; do
		launch_worker $host
	done
}

function kill_worker() {
	ssh -i $THIS_DIR/crust_test.pem ec2-user@$1 'kill $(pgrep -f cbmc)'
}

function kill_all_workers() {
	for host in ${WORKER_IPS[@]}; do
		kill_worker $host
	done
}

function deploy_jh() {
	scp -r -i $THIS_DIR/crust_test.pem $THIS_DIR/../workqueue ubuntu@${JOBHOST}:~/
}

function dashboard() {
	for host in ${WORKER_IPS[@]}; do
		xfce4-terminal --geometry 80x15 --command "ssh -t -i $THIS_DIR/crust_test.pem ec2-user@$host htop"
	done
}

"$@"
