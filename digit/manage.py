#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
Entry point for Battle service management commands
"""
import redis
from rq import Connection, Worker
from flask.cli import FlaskGroup

from test_auth.app import app

cli = FlaskGroup(create_app=lambda x: app)


@cli.command('run_worker')
def run_worker():
    redis_url = app.config['REDIS_URL']
    redis_connection = redis.from_url(redis_url)
    with Connection(redis_connection):
        worker = Worker(app.config['QUEUES'])
        worker.work()


if __name__ == '__main__':
    cli()
