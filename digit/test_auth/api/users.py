import logging
import json

from flask_restx import fields
from flask import request

from test_auth.app import api, ns, redis_store, APP_VERSION
from test_auth.resource import TestResource
from test_auth.api import default_parser
from test_auth.utils import Pbkdf2, json_dump


logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

user_model = api.model('User', {
    'email': fields.String(),
    'name': fields.String(),
    'role': fields.String(),
    'password': fields.String()
})

users_list_resp = api.model('UserListResp', {
    'data': fields.List(fields.Nested(user_model)),
    'version': fields.String(),
})

user_resp = api.model('UserResp', {
    'data': fields.Nested(user_model),
    'version': fields.String()
})

user_chpw_request = api.model('UserChPwReq', {
    'old_password': fields.String(),
    'new_password': fields.String()
})


@ns.route('/users/<string:uid>', endpoint='users')
class Users(TestResource):

    # pylint: disable=no-self-use
    @api.doc(description='Details for a specific user.', params={'uid': 'ID of the user'})
    @api.marshal_with(user_resp)
    def get(self, uid, **kwargs):
        """
        Returns user details.
        """
        user_role = kwargs.get('creds', {}).get('user_role', None)
        user_id = kwargs.get('creds', {}).get('user_id', None)
        if user_role != 'Staff' and user_id != uid:
            api.abort(403, "Access denied.")
        user = redis_store.get("user:{}".format(uid))
        if not user:
            api.abort(400, "User not found.")
        return {'data': user, 'version': APP_VERSION}


@ns.route('/users', endpoint='users_list')
class UsersList(TestResource):
    """
    List of Users
    """
    # pylint: disable=no-self-use
    @api.doc(description='List of users')
    @api.marshal_with(users_list_resp)
    def get(self, **kwargs):
        """
        Returns list of all users.
        """
        user_role = kwargs.get('creds', {}).get('user_role', None)
        if user_role != 'Staff':
            api.abort(403, "Access denied.")
        result = []
        for key in redis_store.scan_iter("user:*"):
            data = redis_store.get(key)
            record = json.loads(data)
            record.pop('password')
            result.append(record)
        return {'data': result, 'version': APP_VERSION}

    def _parse_request(self):
        """
        Parse request parameters
        """
        data = request.json

        if not data:
            # 400: BAD_REQUEST
            api.abort(400,
                      'No JSON request data')
        logger.debug('Request data: %s', data)

    # pylint: disable=no-self-use
    @api.doc(description='Create User',
             body=user_model,
             params={'payload': 'User object'},
             parser=default_parser)
    @api.marshal_with(user_resp)
    def post(self, **kwargs):
        """
        Adds a new user.
        """
        user_role = kwargs.get('creds', {}).get('user_role', None)
        if user_role != 'Staff':
            api.abort(403, "Access denied.")

        email = request.json.get('email')
        name = request.json.get('name')
        role = request.json.get('role')
        password = request.json.get('password')
        if not email:
            api.abort(400, 'Email is a required field.')
        if not name:
            api.abort(400, 'Name is a required field.')
        if not role:
            api.abort(400, 'Role is a required field.')
        if not password:
            api.abort(400, 'Password is a required field.')

        pbkf = Pbkdf2()
        enc_password = pbkf.make_password(password)
        data = request.json
        data['password'] = enc_password
        try:
            redis_store.set("user:{}".format(email), json_dump(data))
        except Exception as exc:
            logger.exception(exc)
            # 400: BAD_REQUEST
            api.abort(400, str(exc))
        response_data = request.json
        response_data.pop('password')
        return {'data': response_data, 'version': APP_VERSION}
