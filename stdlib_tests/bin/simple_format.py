import sys
import re

BRACE_RE = re.compile(r'[{}]')

def adjustment(line):
    adj = 0
    idx = 0
    while idx < len(line):
        m = BRACE_RE.search(line, idx)
        if m is None:
            break
        if m.group() == '{':
            adj += 1
        else:
            adj -= 1
        idx = m.end()
    return adj

def emit(line, depth):
    sys.stdout.write('    ' * depth + line.lstrip())

depth = 0
for line in sys.stdin:
    if len(line.strip()) == 0:
        continue

    adj = adjustment(line)
    if adj > 0:
        emit(line, depth)
        depth += adj
    else:
        depth += adj
        emit(line, depth)
