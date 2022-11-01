from abc import ABCMeta, abstractmethod
from functools import wraps
import logging
import time

from flask import g as flask_g
from flask import abort, jsonify, request
from flask_restx import Resource

from test_auth.app import app
import test_auth.settings as settings

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name


class AuthBackendBase(object):
    __metaclass__ = ABCMeta

    @abstractmethod
    def authenticate(self):
        """Must raise an InvalidToken exception on failure, or
           return authentication token"""

    @abstractmethod
    def check_roles(*roles):
        """Must return True if user is member of at least one group,
           or False otherwise"""

def check_session(session_id):
    """
    Checks if ther's such token in tokens DB.
    """
    result = {
        'session_id': session_id,
        'user_id': None,
        'user_email': None,
        'token_type': 'user'
    }
    if session_id == settings.ADMIN_SECRET:
        result['user_role'] = 'Staff'
        return result
    redis_store.get('session:{}'.format(session_id))
    if not session:
        return None
    user_id = session.get('user_id')
    result['user_id'] = session.get('user_id')

    user = redis_store.get('player:{}'.format(user_id))
    if user:
        result['user_name'] = user['name']
    return result


class TokenAuthBackend(AuthBackendBase):

    def authenticate(self):
        """
        Extract request headers relevant for authentication and
        check token against user tokens db.
        """
        session_id = request.headers.get(settings.SESSION_ID_HDR, None)
        if not session_id:
            return None
        result = check_session(session_id)
        if result:
            return result
        raise InvalidToken("Unrecognized token")

    def check_roles(self, *roles):
        return True


def get_authenticator():
    """
    Instantiate appropriate auth backend and use
    it to authenticate request
    """
    return TokenAuthBackend()


def authenticate(func):
    """
    Resource method decorator

    See
    http://flask-restful.readthedocs.org/en/latest/extending.html#resource-method-decorators
    """
    @wraps(func)
    def wrapper(*args, **kwargs):  # pylint: disable=missing-docstring
        if getattr(func, 'authenticated', False):
            return func(*args, **kwargs)
        try:
            creds = get_authenticator().authenticate()
        except InvalidToken:
            # Just passing on InvalidToken does not work in Docker environment
            abort(401)

        kwargs.update({'creds': creds})
        if creds:
            return func(*args, **kwargs)
        else:
            logger.error('No auth token received')
            abort(401)
    return wrapper


def check_roles(*roles):
    """
    Resource method decorator to check for membership in at least one
    of the specified role.
    This decorator expects a list of roles attached to flask.g.
    """
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):  # pylint: disable=missing-docstring
            user_roles = get_authenticator().check_roles(*roles)

            if user_roles:
                return func(*args, **kwargs)
            else:
                abort(401)
        return wrapper
    return decorator


def limit_content_length(f):
    """
    Decorator to enforce max. length of request content.
    Flask-RESTplus does not enforce this by default
    """
    @wraps(f)
    def wrapper(*args, **kwargs):
        cl = request.content_length
        if cl is not None and cl > app.config['MAX_CONTENT_LENGTH']:
            abort(413)  # REQUEST_ENTITY_TOO_LARGE
        return f(*args, **kwargs)
    return wrapper


# pylint: disable=no-init,too-few-public-methods
class TestResource(Resource):
    """
    REST resource that requires authentication
    """
    # These decorators get applied to all inherited resources.
    # They are called in reverse order
    method_decorators = [
        check_roles(*settings.STANDARD_ROLES),
        authenticate
    ]


class InvalidToken(Exception):
    """
    Raised when the API authentication token is missing or invalid
    """
    status_code = 405  # METHOD_NOT_ALLOWED

    def __init__(self, message):
        Exception.__init__(self)
        self.message = message

    def to_dict(self):
        """Provide error message as dict"""
        result = {}
        result['message'] = self.message
        return result


@app.errorhandler(InvalidToken)
def handle_invalid_token(error):
    """Custom error handler for ``InvalidToken`` exception """
    logger.exception(error)
    response = jsonify(error.to_dict())
    response.status_code = error.status_code
    return response

@app.before_request
def before_request():
    flask_g.start = time.time()

@app.after_request
def after_request(response):
    diff = time.time() - flask_g.start
    if (response.response) and (200 <= response.status_code < 300):
        response.headers['elapsed-time'] = str(round(diff, 3))
    return response
