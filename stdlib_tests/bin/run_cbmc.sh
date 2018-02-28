#!/bin/bash

crate=$(basename "$1")
crate=${crate%.c}

cbmc "$1" -I ../src --function "$crate\$main" --ILP32 --slice-formula



