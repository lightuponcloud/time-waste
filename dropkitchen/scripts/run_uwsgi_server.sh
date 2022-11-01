#!/bin/bash

echo "Starting uwsgi in directory $PWD"

if [ "$UWSGI_RESTART" == "1" ];then
    UWSGI_EXTRA_OPTS+="--py-auto-reload 1"
fi


uwsgi --wsgi-file authapp.py  --callable app \
     --enable-threads \
     --http ${APP_HOST}:${APP_PORT} \
     --processes ${UWSGI_NUM_PROCESSES-2} \
     --threads ${UWSGI_NUM_THREADS-2} \
     ${UWSGI_EXTRA_OPTS} \
     --chunked-input-timeout 60 \
     --socket-timeout 60 \
     --http-timeout 1800 \
     --log-format \
       '{"@timestamp": "%(time)", "method": "%(method)", "uri": "%(uri)", "status": "%(status)", "name": "uWSGI.request", "response_time_ms": "%(msecs)", "vsz": "%(vsz)", "size": "%(size)", "rsize": "%(rsize)", "cl": "%(cl)", "ltime": "%(ltime)", "user": "%(user)", "addr": "%(addr)", "host": "%(host)", "uagent": "%(uagent)", "pid": "%(pid)"}'
