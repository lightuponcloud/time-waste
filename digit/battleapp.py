#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Test Auth web application
"""
import logging

from test_battle.app import app
from test_battle.api import *  # NOQA

logger = logging.getLogger('test_battle')  # pylint: disable=invalid-name

if __name__ == '__main__':
    host = '31.28.168.162'
    port = 8889
    logger.debug('Starting app on %s:%s', host, port)
    app.run(host=host, port=port)
