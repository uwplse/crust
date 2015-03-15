# -*- coding: utf-8 -*-
import json
import os
import re
import sys
from flask import Flask, request, session, g, redirect, url_for, abort, \
     render_template, flash, _app_ctx_stack, make_response
import time

# create our little application :)
app = Flask(__name__)

# configuration
DEBUG = os.environ.get('WQ_DEBUG') is not None
SECRET_KEY = os.environ.get('WQ_SECRET_KEY') or 'development key'

app.config.from_object(__name__)

def outfile(t):
    return os.path.join('results', '%s.txt' % t)

os.makedirs('results', exist_ok=True)

tasks = set()
pending = set()

sat_tasks = set()
sat_pending = set()

def get_pqueue(task):
    if task in pending:
        return pending
    elif task in sat_pending:
        return sat_pending
    else:
        return None

with open('tasks.txt') as f:
    for line in f:
        task = line.strip()
        if not os.path.exists(outfile(task)):
            tasks.add(task)

n_tasks = len(set(tasks))

@app.route('/get_job')
def get_job():
    global tasks, pending

    #if len(tasks) == 0:
    #    tasks = set(pending)

    # If it's STILL zero, then there are no more tasks.
    if len(tasks) == 0:
        abort(404)

    task = tasks.pop()
    pending.add(task)
    return task + ":Z3"

@app.route('/finish_job/<string:task>', methods=['POST'])
def finish_job(task):
    pqueue = get_pqueue(task)
    if pqueue is None:
        return ""

    with open(outfile(task), 'wb') as f:
        f.write(request.data)

    pqueue.remove(task)
    #if task in tasks:
    # Might happen toward the end when jobs are running out
    # tasks.remove(task)

    return ''

@app.route("/queue_sat/<string:task>", methods=["POST"])
def queue_sat(task):
    if task not in pending:
        return ""
    pending.remove(task)
    sat_tasks.add(task)
    return ""

@app.route("/get_sat_job")
def get_sat_job():
    global sat_tasks, sat_pending, tasks, pending
    if len(sat_tasks) != 0:
        sat_task = sat_tasks.pop()
        sat_pending.add(sat_task)
        to_ret = sat_task + ":SAT"
        return to_ret

    return get_job()


if __name__ == '__main__':
    app.run()
