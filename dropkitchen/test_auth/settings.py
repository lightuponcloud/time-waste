# -*- coding: utf-8 -*-

"""Flask application settings common to all execution environments.

   All settings must be in uppercase to be picked up by Flask.

   Values taken from the environment are flagged as such:
   (**environment variable**)
"""
from os import environ as env

# Flask settings
FLASK_SERVER_NAME = 'localhost:8888'
FLASK_DEBUG = True

# Flask-Restplus settings
RESTPLUS_SWAGGER_UI_DOC_EXPANSION = 'list'
RESTPLUS_VALIDATE = True
RESTPLUS_MASK_SWAGGER = False
RESTPLUS_ERROR_404_HELP = False

# ---- Flask builtin configuration parameters ----
JSON_AS_ASCII = False

#: Whether or not Flask should be run in DEBUG mode (**environment variable**)
DEBUG = bool(int(env.get('DEBUG', False)))

# Limit maximum content length to reduce impact of DDOS attacks
MAX_CONTENT_LENGTH = 2 ** 32
