# -*- coding: utf-8 -*-

import logging

import pytest

from test_auth import app as auth_app

# Side effect: API endpoint registration
import authapp  # NOQA

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name


@pytest.fixture
def app():
    """
    Test application fixture
    """
    return authapp.app
