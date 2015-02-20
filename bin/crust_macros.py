import re
import sys
import os.path
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--intrinsics", action="store_true")
parser.add_argument("--module")
parser.add_argument("input_file", nargs="?")

e = parser.parse_args()

input_filename = e.input_file
with_intrinsics = e.intrinsics
add_module = e.module

if input_filename is None:
    input_file = sys.stdin
else:
    input_file = open(input_filename, 'r')

macro_defs = None
with open(os.path.dirname(sys.argv[0]) + "/../src/crust_macros.rs", "r") as f:
    macro_defs = f.read()

intrinsic_defs = None
if with_intrinsics:
    with open(os.path.dirname(sys.argv[0]) + "/../src/crust_intrinsics.rs") as f:
        intrinsic_defs = f.read()

l = input_file.readline()
while l:
    l_t = l.strip()
    if re.match(r'^#!',l_t):
        print l,
    elif re.match(r'^//', l_t):
        print l
    elif len(l_t) == 0:
        print l,
    else:
        print macro_defs,
        if with_intrinsics:
            print intrinsic_defs,
        if add_module:
            print ("mod " + add_module + ";")
        print l,
        break
    l = input_file.readline()

print input_file.read(),
