#!/bin/bash

BIN_DIR=$(dirname $0);

RBMC_FLAGS="-A warnings -L $BIN_DIR/../lib"

$BIN_DIR/rbmc $RBMC_FLAGS "$1"
