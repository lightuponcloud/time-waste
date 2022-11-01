import logging
import uuid
import json
from datetime import datetime, timedelta

from flask_restx import fields
from flask import request

from test_auth.app import api, ns, redis_store, APP_VERSION
from test_auth.api import default_parser
from test_auth.utils import json_dump
from test_auth.resource import TestResource
from test_auth.settings import TOKEN_EXPIRATION_DAYS, ADMIN_SECRET
from test_auth.utils import Pbkdf2


logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

token_model = api.model('Token', {
    'id': fields.String(),
    'issued_at': fields.DateTime(),
    'expires': fields.DateTime(),
    'email': fields.String(),
})

user_auth_request = api.model('Auth', {
    'email': fields.String(),
    'password': fields.String(),
})

token_resp = api.model('TokenResp', {
    'data': fields.Nested(token_model),
    'version': fields.String()
})

app_token_model = api.model('ApplicationToken', {
    'id': fields.String(),
    'token': fields.String(),
    'issued_at': fields.DateTime(),
    'org_name': fields.String(),
})

app_token_resp = api.model('AppTokenResp', {
    'data': fields.Nested(app_token_model),
    'version': fields.String()
})


@ns.route('/tokens', endpoint='tokens')
class Tokens(TestResource):
    """
    Generates a token. Uses the password authentication method.
    """
    def _parse_request(self):
        """
        Parse request parameters
        """
        data = request.json

        if not data:
            # 400: BAD_REQUEST
            api.abort(400, 'No JSON request data')
        logger.debug('Request data: %s', data)

    # pylint: disable=no-self-use
    @api.doc(description='Password authentication',
             params={'payload': 'Token object'},
             body=user_auth_request,
             parser=default_parser)
    @api.marshal_with(token_resp)
    def post(self):
        """
        Returns token if credentials are valid. Otherwise returns 400.
        """
        json_response = request.json
        email = json_response.get('email')
        password = json_response.get('password')
        user = redis_store.get("user:{}".format(email))
        if not user:
            api.abort(400, 'User not found.')
        user = json.loads(user)
        if not password:
            api.abort(400, 'Password is required.')
        token_id = str(uuid.uuid4())
        user_password = user.get('password')
        if not user_password:
            api.abort(400, 'Credentials missing.')
        pbkf = Pbkdf2()
        if not pbkf.check_password(password, user_password):
            api.abort(400, 'Incorrect credentials provided.')
        token_record = {
            'id': token_id,
            'issued_at': datetime.now(),
            'expires': datetime.now() + timedelta(days=TOKEN_EXPIRATION_DAYS),
            'email': str(user['email'])
        }
        try:
            redis_store.set(token_id, json_dump(token_record))
        except Exception as exc:
            logger.exception(exc)
            # 400: BAD_REQUEST
            api.abort(400, str(exc))
        else:
            return {"data": token_record, "version": APP_VERSION}
    post.authenticated = True


def token_parser():
    parser = default_parser()
    parser.add_argument('X-Subject-Token', location='headers')
    return parser


@ns.route('/tokens/action', endpoint='token')
class Token(TestResource):
    @api.doc(description='Returns token info', parser=token_parser, 
             params={'X-Subject-Token': 'Authentication token'})
    def get(self, **kwargs):
        """
        Validates X-Subject-Token.
        """
        subj_token = request.headers.get('X-Subject-Token', None)
        if not subj_token:
            api.abort(403, "X-Subject-Token should be specified.")
        token_type = kwargs.get('creds', {}).get('token_type', None)
        if token_type == 'user':
            if subj_token == ADMIN_SECRET:
                return {'data': "ADMIN TOKEN", "version": APP_VERSION}
            user_id = kwargs.get('creds', {}).get('user_id')
            if user_id is None:
                api.abort(403, "Access denied.")
            user_role = kwargs.get('creds', {}).get('user_role', None)
            token_obj = redis_store.get(subj_token, {})
            if user_role != 'Staff' and token_obj.get('user_id', None) != user_id:
                api.abort(403, "Access denied.")
        else:
            token_obj = check_old_session(subj_token)
        return {'data': token_obj, "version": APP_VERSION}

    @api.doc(description='Revoke token', parser=token_parser,
             params={'X-Subject-Token': 'Authentication token'})
    def delete(self, **kwargs):
        """
        Revokes a token, specified in X-Subject-Token.
        """
        user_role = kwargs.get('creds', {}).get('user_role', None)
        token = request.headers.get('X-Subject-Token', None)
        if not token:
            api.abort(403, "X-Subject-Token should be specified.")
        user_id = kwargs.get('creds', {}).get('user_id')
        if user_id is None:
            api.abort(403, "Access denied.")
        token_obj = redis_store.get(token)
        if user_role != 'Staff' and token_obj.get('user_id', None) != user_id:
            api.abort(403, "Access denied.")
        redis_store.delete(token)
        return {'data': [], "version": APP_VERSION}
