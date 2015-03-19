import os
import pickle
import re
import sys

assert not sys.stdout.isatty()

LINE2_RE = re.compile(r'(.*)user (.*)system (.*):(.*)\.(.*)elapsed (.*)%CPU ' +
        r'\((.*)avgtext+(.*)avgdata (.*)maxresident\)k')
LINE3_RE = re.compile(r'(.*)inputs+(.*)outputs ' +
        r'\((.*)major+(.*)minor\)pagefaults (.*)swaps')

results = {}
for fn in os.listdir('.'):
    with open(fn) as f:
        (a, b, c) = f.readlines()
        mb = LINE2_RE.match(b)
        mc = LINE3_RE.match(c)

        elapsed_min = int(mb.group(3))
        elapsed_sec = int(mb.group(4))
        elapsed_hund = int(mb.group(5))
        elapsed = int(elapsed_min) * 60 + int(elapsed_sec) + float(elapsed_hund) / 100

        r = {
                'status': a,
                'user': float(mb.group(1)),
                'system': float(mb.group(2)),
                'elapsed': elapsed,
                'cpu_percent': float(mb.group(6)),
                'avg_text': int(mb.group(7)),
                'avg_data': int(mb.group(8)),
                'max_resident': int(mb.group(9)),
                'inputs': int(mc.group(1)),
                'outputs': int(mc.group(2)),
                'major_faults': int(mc.group(3)),
                'minor_faults': int(mc.group(4)),
                'swaps': int(mc.group(5)),
                }
        results[fn] = r

pickle.dump(results, sys.stdout, -1)

