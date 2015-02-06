import re
import sys
import os.path

input_file = None
if len(sys.argv) == 0:
    input_file = sys.stdin
else:
    input_file = open(sys.argv[1], 'r')

macro_def_f = os.path.dirname(sys.argv[0]) + "/../src/crust_macros.rs"
macro_defs = open(macro_def_f, 'r').read()

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
        print l,
        break
    l = input_file.readline()

print input_file.read(),
