# -*- coding: utf-8 -*-
# flake8: noqa: F401

import logging

# Import sub-modules for the side effect of endpoint registration
from .app import app, api
from .app import (parse_loglevel, HTTP_STATUS_TOO_MANY_REQUESTS,
                  handle_exception, handle_rate_limit)

logger = logging.getLogger('test')  # pylint: disable=invalid-name

import test_auth.api.endpoints
