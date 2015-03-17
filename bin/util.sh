#!/bin/bash

JOBHOST="54.148.202.229"

WORKER_IPS=("54.148.135.179" "54.148.109.4" "52.11.192.79" "54.68.125.25" "54.148.109.20")

function grep_test() {
	egrep -o 'libtest_[[:digit:]]+\$crust_test_[[:digit:]]+' "$@" | sort | uniq
}

function ec2ssh() {
	ssh -i /home/jtoman/grad_school/research/crust/bin/crust_test.pem "ec2-user@$1"
}

function jhssh() {
	ssh -i /home/jtoman/grad_school/research/crust/bin/crust_test.pem "ubuntu@$JOBHOST"
}
