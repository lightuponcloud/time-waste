# -*- coding: utf-8 -*-

"""
Flask application settings common to all execution environments.

All settings must be in uppercase to be picked up by Flask.

Values taken from the environment are flagged as such:
(**environment variable**)
"""
from os import environ as env

# Flask settings
FLASK_SERVER_NAME = 'localhost:8888'
FLASK_DEBUG = True

# Flask-RestX settings
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

REDIS_HOST = env.get('REDIS_HOST', 'redis_battle')
REDIS_PORT = env.get('REDIS_PORT', 6379)
REDIS_DB = env.get('REDIS_DB', '0')
REDIS_URL = env.get('REDIS_URL', 'redis://{}:{}/{}'.format(
    REDIS_HOST, REDIS_PORT, REDIS_DB))


STANDARD_ROLES = ['Staff', 'User']

SESSION_ID_HDR = 'authorization'
TOKEN_EXPIRATION_DAYS = 1
ADMIN_SECRET = env.get("ADMIN_SECRET", "IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct")

# The following URL is used to authenticate tokens
AUTH_ENDPOINT = env.get('AUTH_ENDPOINT',
                        'http://auth_service:5000/v1/auth/tokens/action')

# the following URL is used to lookup users
PLAYERS_ENDPOINT = env.get('PLAYERS_ENDPOINT',
                           'http://auth_service:5000/v1/auth/player/{}')

LEADERBOARD_ENDPOINT = env.get('LEADERBOARD_ENDPOINT',
                               'http://auth_service:5000/v1/auth/players')
