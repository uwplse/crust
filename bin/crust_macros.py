import re
import sys
import os.path

input_file = None

with_nondet = False

if (len(sys.argv) > 1 and sys.argv[1] == '--nondet'):
    with_nondet = True
    del sys.argv[1]

if len(sys.argv) == 1:
    input_file = sys.stdin
else:
    input_file = open(sys.argv[1], 'r')

macro_def_f = os.path.dirname(sys.argv[0]) + "/../src/crust_macros.rs"
macro_defs = open(macro_def_f, 'r').read()

nondet_defs = None
if with_nondet:
    with open(os.path.dirname(sys.argv[0]) + "/../src/crust_nondet.rs") as f:
        nondet_defs = f.read()

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
        if with_nondet:
            print nondet_defs,
        print l,
        break
    l = input_file.readline()

print input_file.read(),
