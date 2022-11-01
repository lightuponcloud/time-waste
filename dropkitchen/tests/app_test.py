# -*- coding: utf-8 -*-

import logging

import pytest
from flask import url_for

import test_auth as sut


def test_missing_token(config, app, client):
    config.debug = False
    response = client.get(url_for('api.users', uid='0'))
    assert response.status_code == 401


@pytest.mark.parametrize('log_level_str,expected', [
    ('DEBUG', logging.DEBUG),
    ('INFO', logging.INFO),
    ('WARNING', logging.WARNING),
    ('ERROR', logging.ERROR),
    ('FATAL', logging.FATAL),
    (' DEBUG ', logging.DEBUG),
])
def test_parse_loglevel_valid(log_level_str, expected):
    assert sut.parse_loglevel(log_level_str) == expected


@pytest.mark.parametrize('log_level_str', [
    '',
    'UNKNOWN',
    None
])
def test_parse_loglevel_invalid(log_level_str):
    with pytest.raises(ValueError):
        sut.parse_loglevel(log_level_str)


def test_handle_exception(request_ctx):
    response = sut.handle_exception(Exception('testmessage'))
    assert response.status_code == 500


def test_handle_ratelimit_exceeded(request_ctx):
    response = sut.handle_rate_limit(Exception('testmessage'))
    assert response.status_code == sut.HTTP_STATUS_TOO_MANY_REQUESTS
