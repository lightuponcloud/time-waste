#!/bin/bash
export PYTHONPATH=/digit

gunicorn test_auth:app --workers 10 --bind 0.0.0.0:5000 \
         --access-logformat "%(t)s","method: %(m)s","uri: %(U)s","status: %(s)s","response_time: %(D)s","response_size: %(B)s","addr: %(h)s","uagent: %(a)s","pid: %(p)s" \
         --log-level info --access-logfile - --error-logfile -
