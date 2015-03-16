#!/bin/bash

function grep_test() {
	egrep -o '(libtest_[[:digit:]]+\$)?crust_test_[[:digit:]]+' "$@" | sort | uniq
}

function ec2ssh() {
	ssh -i /home/jtoman/grad_school/research/crust/bin/crust_test.pem "ec2-user@$1"
}

function jhssh() {
	ssh -i /home/jtoman/grad_school/research/crust/bin/crust_test.pem "ubuntu@52.10.62.245"
}

WORKER_IPS=("54.69.23.204" "54.69.79.5" "54.69.39.4" "52.11.125.57" "54.68.27.160" "52.11.150.227" "54.68.163.64" "54.68.82.25")

JOBHOST="52.10.62.245"
