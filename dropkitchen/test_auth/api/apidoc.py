# -*- coding: utf-8 -*-
"""
API documentation endpoint
"""

from flask import Blueprint
from flask_restplus import apidoc

from test_auth.app import app, api, APP_PATH_PREFIX
from test_auth.resource import authenticate

meta_blueprint = Blueprint('meta', __name__)


@api.documentation
@authenticate
def swagger_ui():
    """
    Serve API documentation
    """
    result = apidoc.ui_for(api)

    # Patch swaggerui URL to use reverse proxy prefix - apparently
    # no clean way of doing this.
    return result.replace(
        '"/swaggerui', '"{0}/swaggerui'.format(APP_PATH_PREFIX))


app.register_blueprint(meta_blueprint)
