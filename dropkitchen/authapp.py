#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Test Auth web application
"""
import logging

from test_auth.app import app
from test_auth.api import *  # NOQA

logger = logging.getLogger('test_auth')  # pylint: disable=invalid-name

if __name__ == '__main__':
    host = '31.28.168.164'
    port = 8888
    logger.debug('Starting app on %s:%s', host, port)
    app.run(host=host, port=port)
