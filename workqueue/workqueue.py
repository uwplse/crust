# -*- coding: utf-8 -*-
import json
import os
import re
import sys
from flask import Flask, request, session, g, redirect, url_for, abort, \
     render_template, flash, _app_ctx_stack, make_response


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
with open('tasks.txt') as f:
    for line in f:
        task = line.strip()
        if not os.path.exists(outfile(task)):
            tasks.add(task)

@app.route('/get_job')
def get_job():
    global tasks, pending

    if len(tasks) == 0:
        tasks = set(pending)

    # If it's STILL zero, then there are no more tasks.
    if len(tasks) == 0:
        abort(404)

    task = tasks.pop()
    pending.add(task)
    return task

@app.route('/finish_job/<string:task>', methods=['POST'])
def finish_job(task):
    if task not in pending:
        return ''

    with open(outfile(task), 'wb') as f:
        f.write(request.data)

    pending.remove(task)
    if task in tasks:
        # Might happen toward the end when jobs are running out
        tasks.remove(task)

    return ''

if __name__ == '__main__':
    app.run()
