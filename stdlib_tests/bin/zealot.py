import os
import re
import subprocess
import sys

BRACE = re.compile('[{}]')
FN = re.compile(r'\bfn\b')
NEWLINE = re.compile('\n')
ERR_LOC = re.compile(r'rs:([0-9]+):([0-9]+): ')

def mk_fn_index(s):
    index = []
    pos = 0
    while True:
        fn = FN.search(s, pos)
        if fn is None:
            break
        start = fn.start()

        m = BRACE.search(s, start)
        bpos = m.end()
        level = 1

        while level > 0:
            m = BRACE.search(s, bpos)
            if m.group() == '{':
                level += 1
            else:
                level -= 1
            bpos = m.end()
        end = bpos

        index.append((start, end))
        pos = end

    return index

def write_flat(s, f):
    idx = mk_fn_index(s)
    pos = 0
    for (start, end) in idx:
        f.write(''.join((s[pos:start], '\n', s[start:end].replace('\n', ' '), '\n')))
        pos = end
    f.write(''.join((s[pos:], '\n')))

def mk_line_index(s):
    pos = 0
    index = []
    while True:
        index.append(pos)
        m = NEWLINE.search(s, pos)
        if m is None:
            break
        pos = m.end()
    return index

def run_compiler(args):
    sys.stderr.write('  invoke: %s\n' % (args,))
    p = subprocess.Popen(args, stderr=subprocess.PIPE)
    s = p.stderr.read()
    p.wait()
    sys.stderr.write('  return code: %d\n' % p.returncode)
    sys.stderr.write(s)

    pos = 0
    errs = []
    while True:
        m = ERR_LOC.search(s, pos)
        if m is None:
            break
        pos = m.end()
        errs.append((int(m.group(1)), int(m.group(2))))

    return (p.returncode, errs)

def main(args):
    filename = args[-1]
    tmp_filename = os.path.join(os.path.dirname(filename),
            'z_' + os.path.basename(filename))

    with open(filename, 'r') as f:
        s = f.read()

    with open(tmp_filename, 'w') as f:
        write_flat(s, f)

    mod_args = tuple(args[:-1]) + (tmp_filename,)

    with open(tmp_filename, 'r+') as f:
        idx = mk_line_index(f.read())

        i = 0
        while True:
            sys.stderr.write('round %d\n' % i)
            i += 1

            (ret, errs) = run_compiler(mod_args)
            if ret == 0:
                sys.stderr.write('success!\n')
                break
            assert len(errs) > 0, "failed to detect compiler errors"
            sys.stderr.write('  %d errors\n' % len(errs))

            for line, col in errs:
                f.seek(idx[line - 1])
                f.write('//')
            f.flush()

if __name__ == '__main__':
    main(sys.argv[1:])
