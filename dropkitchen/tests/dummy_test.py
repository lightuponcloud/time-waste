# -*- coding: utf-8 -*-

from flask import g as flask_g

from test_auth.resource import DummyAuthBackend as sut


def test_dummy(request_ctx):
    auth_backend = sut.DummyAuthBackend()
    with request_ctx:
        assert auth_backend.authenticate() is True
        assert flask_g.session_id == 'dummy_session_id'
