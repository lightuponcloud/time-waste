import logging.config

from flask import Flask, Blueprint, jsonify, request
from flask_restx import Api, apidoc
from flask_redis import FlaskRedis

from test_battle.settings import FLASK_DEBUG, REDIS_URL

APP_VERSION = '1'
APP_PATH_PREFIX = '/v{}/battle'.format(APP_VERSION)


class StripPrefixMiddleware(object):
    """
    Remove path prefix added by reverse proxy to ensure
    Flask and Flask-RESTplus the same path configuration works
    in local development and when deployed to AWS
    """

    def __init__(self, app, prefix, replacement):
        self.app = app
        self.prefix = prefix
        self.replacement = replacement

    def __call__(self, environ, start_response):
        """
        Strip prefix
        """
        old_path_info = environ['PATH_INFO']
        new_path_info = old_path_info.replace(self.prefix, self.replacement)
        if new_path_info != old_path_info:
            log.info('Mapping path %s --> %s', old_path_info, new_path_info)
        environ['PATH_INFO'] = new_path_info
        return self.app(environ, start_response)


apidoc.apidoc._static_url_path = '/v2/auth/swagerui'
app = Flask(__name__)
log = logging.getLogger("test_battle")


# Swagger UI static files are normally served at /swaggerui; this does
# not work with reverse proxy serving app with a path prefix
app.wsgi_app = StripPrefixMiddleware(
    app.wsgi_app,
    prefix='{0}/swaggerui'.format(APP_PATH_PREFIX),
    replacement='/swaggerui')

app.config['REDIS_URL'] = REDIS_URL
app.config['QUEUES'] = ["default"]

redis_store = FlaskRedis(app)


@app.after_request
def set_cors_header(response):
    """
    Set CORS header to allow cross-domain request from the frontend
    """
    response.headers['Access-Control-Allow-Origin'] = '*'

    access_control_req_header = request.headers.get(
        'Access-Control-Request-Headers', None)
    if access_control_req_header:
        response.headers[
            'Access-Control-Allow-Headers'] = access_control_req_header
    return response


@app.errorhandler(Exception)
def handle_exception(error):
    """
    Custom error handler for a generic exception
    """
    if app.config['DEBUG']:
        error_msg = str(error)
    else:
        # Don't reveal internals
        error_msg = 'Internal server error'

    response = jsonify({
        'message': error_msg
    })
    log.exception(error_msg)
    response.status_code = 500
    return response


def parse_loglevel(log_level):
    log_level = str(log_level).strip()
    if log_level not in ('DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'):
        raise ValueError('Unknown log level "{0}"'.format(log_level))
    return getattr(logging, log_level)


# Load application settings
app.config.from_object('test_battle.settings')

authorizations = {
    'apikey': {
        'type': 'apiKey',
        'in': 'header',
        'name': 'authorization',
    }
}

blueprint = Blueprint('api', __name__)
Api.specs_url = f'{APP_PATH_PREFIX}/swagger.json'
api = Api(None, validate=True, authorizations=authorizations, security='apikey')
api.app = blueprint
api.init_app(blueprint, api_doc='/', add_specs=True)
app.register_blueprint(blueprint, url_prefix=APP_PATH_PREFIX)
app.config.SWAGGER_UI_JSONEDITOR = True
app.config.BUNDLE_ERRORS = True

ns = api


def main():
    log.info(('Starting development server at '
             'http://{}/api/ <<<<<').format(app.config['SERVER_NAME']))
    app.run(debug=settings.FLASK_DEBUG)


if __name__ == "__main__":
    log.debug('Starting app')
    main()
