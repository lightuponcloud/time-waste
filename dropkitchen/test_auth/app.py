import logging.config

from flask import Flask, Blueprint, jsonify, request
from flask_restplus import Api
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address

from flask_sqlalchemy import SQLAlchemy


APP_PATH_PREFIX = '/api/auth/v1'
HTTP_STATUS_TOO_MANY_REQUESTS = 429


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


app = Flask(__name__)
log = logging.getLogger("test_auth")
limiter = Limiter(
    app,
    key_func=get_remote_address,
    default_limits=["100 per minute"],
)

# Swagger UI static files are normally served at /swaggerui; this does
# not work with reverse proxy serving app with a path prefix
app.wsgi_app = StripPrefixMiddleware(
    app.wsgi_app,
    prefix='{0}/swaggerui'.format(APP_PATH_PREFIX),
    replacement='/swaggerui')

app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:////tmp/test.db'
db = SQLAlchemy(app)

# Since there are only 3 tables, I define them here

class Recipe(db.Model):
    """
    Stores information on recipies.
    """
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False)
    intro = db.Column(db.Text, nullable=True)
    total_calories = db.Column(db.Integer)

    def __unicode__(self):
        return self.name

    def _get_ingredients(self):
        """
        Executes INNER JOIN to retrieve recipe ingredients
        using intermediary table.
        """
        return db.session.query(Ingredient).join(
            RecipeIngredient,
            Ingredient.id == RecipeIngredient.ingredient_id
        ).filter_by(recipe_id=self.id)
    ingredients = property(_get_ingredients)


class Ingredient(db.Model):
    """
    Stores information on recipe ingredients.
    """
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, unique=True, nullable=False)
    value100gm = db.Column(db.Integer(), nullable=False)

    def __unicode__(self):
        return self.name


class RecipeIngredient(db.Model):
    """
    Stores relation between Recipe and Ingredient
    """
    recipe_id = db.Column(db.Integer, db.ForeignKey('recipe.id'), nullable=False, primary_key=True)
    ingredient_id = db.Column(db.Integer, db.ForeignKey('ingredient.id'), nullable=False, primary_key=True)

    def __unicode__(self):
        recipe = Recipe.query.filter_by(id=self.recipe_id).first()
        ingredient = Ingredient.query.filter_by(id=self.ingredient_id).first()
        return "{} {}".format(unicode(recipe), unicode(ingredient))


db.create_all()
db.session.commit()


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


@app.errorhandler(HTTP_STATUS_TOO_MANY_REQUESTS)
def handle_rate_limit(error):
    log.exception(error)
    response = jsonify({
        'message': "ratelimit exceeded %s" % error
    })
    response.status_code = HTTP_STATUS_TOO_MANY_REQUESTS
    return response


def parse_loglevel(log_level):
    log_level = str(log_level).strip()
    if log_level not in ('DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'):
        raise ValueError('Unknown log level "{0}"'.format(log_level))
    return getattr(logging, log_level)


# Load application settings
app.config.from_object('test_auth.settings')

blueprint = Blueprint('api', __name__)
api = Api(None, validate=True)
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
