import multiprocessing
import Queue
import subprocess
import argparse
import os.path
import sys
import logging
import time
import re
import tempfile
import os
import signal
import urllib
import urllib2

#logger = multiprocessing.log_to_stderr()
#logger.setLevel(logging.DEBUG)

cbmc_binary = None
unwind_bound = None
include_dir = None
use_z3 = False
timeout = 120
job_host = None
n_workers = multiprocessing.cpu_count()
local = True
test_source = None

SUCC = 0
FAIL = 1
TIMEOUT = 2
BAD_TRANS = 3

loop_re = re.compile(r'^Loop (.+):$')
failed_re = re.compile(r'VERIFICATION FAILED\n$')

builtin_loops = re.compile(r'^(core\$ptr\$|core\$intrinsics\$(?!copy_memory)|memmove|memcpy)')

def find_loops(test_file, test_case_name):
    command = [ cbmc_binary, "-I", include_dir, "--show-loops", "--function", test_case_name, test_file ]
    child_proc = subprocess.Popen(command, stdout = subprocess.PIPE)
    child_proc.wait()
    if child_proc.returncode != 6:
        raise Exception("bad exit from loop query " + str(child_proc.returncode))
    loops = []
    for l in child_proc.stdout:
        l = l.strip()
        m = loop_re.match(l)
        if m is None:
            continue
        loop_name = m.group(1)
        #print "found loop: " + loop_name
        if builtin_loops.match(loop_name):
            continue
        loops.append(loop_name)
    if len(loops) == 0:
        return []
    return ["--unwindset", ",".join([ l + ":" + str(unwind_bound) for l in loops ]) ]

dev_null = open("/dev/null", "w")

def kill_z3(proc):
    try:
        os.kill(proc.pid, signal.SIGSTOP)
    except OSError:
        proc.terminate()
        return
    try:
        sh_pid = subprocess.check_output(["pgrep", "-P", str(proc.pid)]).strip()
        z3_pid = subprocess.check_output(["pgrep", "-P", sh_pid])
        os.kill(int(z3_pid), signal.SIGKILL)
    except subprocess.CalledProcessError:
        pass
    except OSError:
        pass
    proc.kill()

def do_run(test_file, test_case_name, unwinding, solve_z3):
    time_stats = tempfile.NamedTemporaryFile(mode="rw", delete = True)
    def read_stats():
        time_stats.seek(0)
        return time_stats.read()
    command = [ "time", "-o", time_stats.name, 
                cbmc_binary, "--pointer-check", "--bounds-check" ] + \
              unwinding + \
              [ "-I", include_dir, "--ILP32", "--slice-formula" ] + \
              (["--z3"] if solve_z3 else []) + \
              ["--function", test_case_name, test_file]
    if solve_z3:
        out = tempfile.NamedTemporaryFile(mode="rw", delete = True)
    else:
        out = dev_null
    child_proc = subprocess.Popen(command, stdout = out)
    start = time.time()
    now = time.time()
    done = False
    while (now - start) < timeout:
        if child_proc.poll() is not None:
            done = True
            break
        time.sleep(1)
        now = time.time()
    if not done:
        if solve_z3:
            kill_z3(child_proc)
        else:
            child_proc.kill()
        child_proc.wait()
        return (TIMEOUT, read_stats())
    stats = read_stats()
    if solve_z3 and child_proc.returncode != 0:
        out.seek(0)
        stdout = out.read()
        if failed_re.search(stdout) is not None:
            return (FAIL, stats)
        else:
            return (BAD_TRANS, stats)
    elif child_proc.returncode != 0:
        return (FAIL, stats)
    else:
        return (SUCC, stats)

def run_test_case(test_file, test_case_name, use_z3):
    unwinding = find_loops(test_file, test_case_name)
    (status, stats) = do_run(test_file, test_case_name, unwinding, use_z3)
    return (status, stats)

def get_job_url(action, job_id):
    return action + "/" + urllib.quote(job_id, "")

def finish_job(job_id, response):
    req = urllib2.Request(job_host + "/finish_job/" + job_id, response)
    req.add_header('Content-Type', 'text/plain')
    urllib2.urlopen(req)

def do_remote_job():
    global use_z3
    job_url = "/get_job" if use_z3 else "/get_sat_job"
    u = urllib.urlopen(job_host + job_url)
    if u.getcode() == 404:
        return False
    (filename,test,solver) = u.read().split(":")
    job = (filename,test)
    job_id = urllib.quote(filename + ":" + test,"")
    if solver == "SAT" and use_z3:
        response = "FAILED"
        response += "\ngot SAT job " + str(job) + " when in z3 mode"
        finish_job(job_id, response)
        return True

    solver_z3 = solver == "Z3"
    (status, stats) = run_test_case(filename, test, solver_z3)
    if status == TIMEOUT:
        response = "TIMEOUT\n" + stats
        finish_job(job_id, response)
    elif status == BAD_TRANS:
        urllib.urlopen(job_host + "/queue_sat/" + job_id, "")
    elif status == FAIL:
        response = "FAILED\n" + stats
        finish_job(job_id, response)
    elif status == SUCC:
        response = "SUCCESS\n" + stats
        finish_job(job_id, response)
    return True
        
        

def remote_worker_thread():
    keep_going = True
    while keep_going:
        keep_going = do_remote_job()

def local_worker_thread(control_pipe, notify_queue, work_queue):
    while True:
        if control_pipe.poll():
            control_pipe.recv()
            notify_queue.put(('quit', multiprocessing.current_process().pid, multiprocessing.current_process().name))
            return True
        work = None
        try:
            work = work_queue.get(False)
        except Queue.Empty:
            pass
        except Exception as e:
            notify_queue.put(('error', multiprocessing.current_process().pid,e,multiprocessing.current_process().name))
            return False
        if work is None:
            notify_queue.put(("done", multiprocessing.current_process().pid,multiprocessing.current_process().name))
            return False
        result = None
        try:
            (result, _) = run_test_case(*(work + [use_z3]))
        except Exception as e:
            notify_queue.put(("error", multiprocessing.current_process().pid,e,multiprocessing.current_process().name))
            return False

        if result == FAIL:
            notify_queue.put(('fail', multiprocessing.current_process().pid,work,multiprocessing.current_process().name))
        elif result == TIMEOUT:
            notify_queue.put(('timeout', multiprocessing.current_process().pid,work,multiprocessing.current_process().name))
        elif result == BAD_TRANS:
            print str(work) + " FAILED TO TRANSLATE"

def parse_args():
    this_dir = os.path.dirname(sys.argv[0])
    parser = argparse.ArgumentParser(description = 'run cbmc tests in parallel')
    parser.add_argument("--cbmc", action="store", default="cbmc")
    parser.add_argument("-I", dest="include_dir", action="store", default=os.path.join(os.path.join(this_dir, ".."), "src"))
    parser.add_argument("--unwind", action="store", type = int)
    parser.add_argument("--z3", action="store_true")
    parser.add_argument("--timeout", action="store", type = int, default = 120)
    parser.add_argument("--nworkers", action="store", type = int, default = multiprocessing.cpu_count())
    parser.add_argument("--job-host", dest="job_host", action="store")
    parser.add_argument("test_names", action="store", nargs = "?")
    parser.add_argument("--worker", action="store_true", default = False)

    opts = parser.parse_args()
    global cbmc_binary
    global unwind_bound
    global include_dir
    global use_z3
    global timeout
    global job_host
    global local
    global test_source

    cbmc_binary = opts.cbmc
    unwind_bound = str(opts.unwind)
    include_dir = opts.include_dir
    use_z3 = opts.z3
    timeout = opts.timeout
    n_workers = opts.nworkers
    if opts.worker:
        local = False
    if not local and opts.job_host is None:
        parser.print_usage()
        sys.exit(-1)
    job_host = opts.job_host
    test_source = opts.test_names

def run_workers():
    workers = []
    for i in range(0, n_workers):
        worker = multiprocessing.Process(target=remote_worker_thread, args=())
        worker.start()
        workers.append(worker)
    while len(workers) != 0:
        workers[0].join()
        workers = workers[1:]

def run_master():
    test_names = []
    def slurp_tests(f):
        for l in sys.stdin:
            if len(l.strip()) == 0:
                continue
            else:
                test_names.append(l.strip().split(":"))

    if test_source is None:
        slurp_tests(sys.stdin)
    else:
        with open(test_source, "r") as f:
            slurp_tests(f)

    work_queue = multiprocessing.Queue(len(test_names))
    notify_queue = multiprocessing.Queue(n_workers * 2)
    p_map = {}
    for t in test_names:
        work_queue.put(t, False)
    num_tests = len(test_names)
    test_names = None
    for i in range(0, n_workers):
        parent_conn, child_conn = multiprocessing.Pipe(True)
        worker_p = multiprocessing.Process(target = local_worker_thread, args = (child_conn,notify_queue, work_queue))
        worker_p.start()
        worker_pid = worker_p.pid
        p_map[worker_pid] = (worker_p, parent_conn)

    def cleanup_procs():
        for (pid,(proc,control_conn)) in p_map.items():
            if not proc.is_alive():
                proc.join()
                del p_map[pid]
                continue
#            print "cleaning up: " + str(pid)
            try:
                control_conn.send(True)
            except IOError:
                continue

    start_time = time.time()
    def print_progress():
        curr_time = time.time()
        msg = "Elapsed: " + str(curr_time - start_time)
        total_tests = str(num_tests)
        processed_tests = num_tests - work_queue.qsize()
        print msg
        print "Processed approx " + str(processed_tests) + " of " + total_tests

    while len(p_map):
        try:
            msg = notify_queue.get(True, 15)
        except Queue.Empty:
            print_progress()
            continue
        if msg[1] not in p_map:
            continue
        if msg[0] == "timeout":
            print "Test case " + str(msg[2]) + " timeout"
            continue
        elif msg[0] == "fail":
            print "Test case " + str(msg[2]) + " failed!"
            continue
        (proc,_) = p_map[msg[1]]
        proc.join()
        del p_map[msg[1]]
        if msg[0] == 'quit' or msg[0] == 'done':
            pass
        elif msg[0] == 'error':
            print "Error while executing tests " + str(msg[2])
            cleanup_procs()

def run_tests():
    global local
    parse_args()
    if local:
        run_master()
    else:
        run_workers()

if __name__ == '__main__':
    run_tests()

