# -*- coding: utf-8 -*-
# flake8: noqa: F401

import logging

# Import sub-modules for the side effect of endpoint registration
from .app import app, api
from .app import (parse_loglevel, handle_exception)

logger = logging.getLogger('test')  # pylint: disable=invalid-name

import test_battle.api.endpoints
