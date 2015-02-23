import multiprocessing
import Queue
import subprocess
import argparse
import os.path
import sys
import logging
import time

#logger = multiprocessing.log_to_stderr()
#logger.setLevel(logging.DEBUG)

cbmc_binary = None
unwind_bound = None
include_dir = None
use_z3 = False

def run_test_case(test_file, test_case_name):
    dev_null = open("/dev/null", "w")
    ret_code = subprocess.call([ cbmc_binary, "--pointer-check", "--bounds-check", 
                                 "-I", include_dir, "--unwind", unwind_bound, 
                                 "--z3" if use_z3 else "", 
                                 "--function", test_case_name, 
                                 test_file], 
                               stdout = dev_null, stderr = subprocess.STDOUT)
    return ret_code


def worker_thread(control_pipe, notify_queue, work_queue):
    _worker_thread(control_pipe, notify_queue, work_queue)

def _worker_thread(control_pipe, notify_queue, work_queue):
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
            result = run_test_case(*work)
        except Exception as e:
            notify_queue.put(("error", multiprocessing.current_process().pid,e,multiprocessing.current_process().name))
            return False

        if result != 0:
            notify_queue.put(('fail', multiprocessing.current_process().pid,work,multiprocessing.current_process().name))
            return False

def run_master():
    this_dir = os.path.dirname(sys.argv[0])
    
    parser = argparse.ArgumentParser(description = 'run cbmc tests in parallel')
    parser.add_argument("--cbmc", action="store", default="cbmc")
    parser.add_argument("-I", dest="include_dir", action="store", default=os.path.join(os.path.join(this_dir, ".."), "src"))
    parser.add_argument("--unwind", action="store", type = int)
    parser.add_argument("--z3", action="store_true")
    parser.add_argument("test_names", action="store", nargs = "?")

    opts = parser.parse_args()
    global cbmc_binary
    global unwind_bound
    global include_dir
    global use_z3

    cbmc_binary = opts.cbmc
    unwind_bound = str(opts.unwind)
    include_dir = opts.include_dir
    use_z3 = opts.use_z3
    
    test_names = []
    def slurp_tests(f):
        for l in sys.stdin:
            if len(l.strip()) == 0:
                continue
            else:
                test_names.append(l.strip().split(":"))

    if opts.test_names is None:
        slurp_tests(sys.stdin)
    else:
        with open(opts.test_names, "r") as f:
            slurp_tests(f)

    n_workers = multiprocessing.cpu_count()
    work_queue = multiprocessing.Queue(len(test_names))
    notify_queue = multiprocessing.Queue(n_workers * 2)
    p_map = {}
    for t in test_names:
        work_queue.put(t, False)
    num_tests = len(test_names)
    test_names = None
    for i in range(0, n_workers):
        parent_conn, child_conn = multiprocessing.Pipe(True)
        worker_p = multiprocessing.Process(target = worker_thread, args = (child_conn,notify_queue, work_queue))
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
        (proc,_) = p_map[msg[1]]
        proc.join()
        del p_map[msg[1]]
        if msg[0] == 'quit' or msg[0] == 'done':
            continue
        elif msg[0] == 'error':
            print "Error while executing tests " + str(msg[2])
        elif msg[0] == 'fail':
            print "Test case " + str(msg[2]) + " failed!"
        cleanup_procs()


if __name__ == '__main__':
    run_master()

