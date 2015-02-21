import sys

input_file = sys.argv[1]
failing_lines = set([ int(s) for s in sys.argv[2].split(",") ])

def readline(i, f):
    l = f.readline()
    return (i + 1,l)

def consume_function(i, f, l):
    string_buf = ""
    has_error = False
    while l:
        string_buf = string_buf + l
        if i in failing_lines:
            has_error = True
        if l == "}\n":
            if has_error:
                # do nothing
                return i
            else:
                print string_buf,
                return i
        (i, l) = readline(i,f)
    return i

with open(input_file, "r") as f:
    (i, l) = readline(0, f)
    while l:
        if l.startswith("fn crust_test_"):
            i = consume_function(i, f, l)
        else:
            print l,
        (i,l) = readline(i, f)


