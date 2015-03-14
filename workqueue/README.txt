Install python3 + python3-flask

Run with:
    WQ_DEBUG=1 python3 workqueue.py

Server reads job names from 'tasks.txt' and writes logs for finished jobs to 'results/'.

Request a job:
    JOB=$(curl http://hostname:5000/next_job)

Send back logs from a job:
    curl -H 'Content-Type: text/plain' --data @logfile.txt \
        http://hostname:5000/finish_job/$JOB
