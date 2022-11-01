# -*- coding: utf-8 -*-

import httplib
import json

import pytest

from cpic.perms.auth.decorators import authenticate
import cpic.perms.auth as sut


@pytest.fixture
def invalid_token():
    return sut.InvalidToken('testmsg')


@pytest.mark.options(CPIC_AUTH_BACKEND='dummy')
def test_authenticate_decorator(request_ctx):

    @authenticate
    def a_handler():
        pass

    with request_ctx:
        a_handler()


def test_invalid_token(invalid_token):
    assert invalid_token.message == 'testmsg'


def test_invalid_token_to_dict(invalid_token):
    assert invalid_token.to_dict() == {'message': 'testmsg'}


def test_handle_invalid_token(invalid_token, request_ctx):
    with request_ctx:
        response = sut.handle_invalid_token(invalid_token)
    assert response.status_code == httplib.METHOD_NOT_ALLOWED
    parsed = json.loads(response.data)
    assert parsed == {'message': 'testmsg'}
