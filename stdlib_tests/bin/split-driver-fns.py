import re
import sys

DRIVER_RE = re.compile(r'^fn priv [^ ]*\$(__crust_test_[0-9]+) ')
INIT_RE = re.compile(r'^fn pub [^ ]*\$crust_init ')

DRIVER_CALL_RE = re.compile(r'call [^ ]*\$(__crust_test_[0-9]+)')

CHUNK_SIZE = 300

def main(ir_file, out_prefix):
    with open(ir_file) as f:
        lines = f.readlines()

    init_lines = []
    driver_lines = []
    other_lines = []
    for line in lines:
        m = DRIVER_RE.match(line)
        if m is not None:
            driver_lines.append((line, m.group(1)))
        elif INIT_RE.match(line) is not None:
            init_lines.append(line)
        else:
            other_lines.append(line)

    other_text = ''.join(other_lines)
    other_lines = None


    def repl(keep_drivers, m):
        if m.group(1) in keep_drivers:
            return m.group()
        else:
            return 'call __crust$unreachable'

    for i in range(0, len(driver_lines), CHUNK_SIZE):
        sys.stderr.write('%d / %d...\n' % (i, len(driver_lines)))
        with open('%s-%d.ir' % (out_prefix, i / CHUNK_SIZE), 'w') as f:
            f.write(other_text)
            f.write(''.join(l for l,_ in driver_lines[i : i + CHUNK_SIZE]))
            keep_drivers = set(n for _,n in driver_lines[i : i + CHUNK_SIZE])
            for l in init_lines:
                f.write(DRIVER_CALL_RE.sub(lambda m: repl(keep_drivers, m), l))

if __name__ == '__main__':
    ir_file, out_prefix = sys.argv[1:]
    main(ir_file, out_prefix)
