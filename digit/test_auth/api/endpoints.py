import logging
import redis
import uuid
import json

from flask import request
from flask import g as flask_g
from flask_restx import fields

from test_auth.app import api, ns, redis_store, APP_VERSION
from test_auth.resource import TestResource
from test_auth.api import default_parser
from test_auth.utils import json_dump

import test_auth.settings as settings

from .spec import player_model, score_model

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

#
# The following response model will allow to tag responses with versions of API
#
player_resp = api.model('PlayerResp', {
    'data': fields.Nested(player_model),
    'version': fields.String(),
})


class BaseEndpoint(TestResource):
    """
    Common class for Recipe endpoints.
    """
    def _parse_request(self):
        """
        Parse request parameters.
        """
        data = request.json

        if not data:
            # 400: BAD_REQUEST
            api.abort(400, 'No JSON request data')
        logger.debug('Request data: %s', data)

    def validate_player(self, record):
        """
        Creates recipe ingredients records in persistent DB.
        """
        name = record.get('name')
        desc = record.get('description')
        gold = record.get('gold')
        silver = record.get('silver')
        if name and len(name)> 20:
            api.abort(400, 'Player name should be less than 20 characters.')
        if desc and len(desc) > 1000:
            api.abort(400, 'Player description is too long.')
        if silver is not None and silver > 1000000:
            api.abort(400, 'Silver value is too big.')
        if gold is not None and gold > 1000000:
            api.abort(400, 'Gold value is too big.')

    def save_player(self, uid, player):
        name = request.json.get('name', None)
        if name:
            player['name'] = name
        desc = request.json.get('description', None)
        if desc:
            player['description'] = desc
        gold = request.json.get('gold', None)
        if gold is not None:
            player['gold'] = gold
        silver = request.json.get('silver', None)
        if silver is not None:
            player['silver'] = silver
        attack_value = request.json.get('attack_value', None)
        if attack_value:
            player['attack_value'] = attack_value
        hit_points = request.json.get('hit_points', None)
        if hit_points:
            player['hit_points'] = hit_points
        luck_value = request.json.get('luck_value', None)
        if luck_value:
            player['luck_value'] = luck_value
        try:
            redis_store.set("player:{}".format(uid), json_dump(player))
        except Exception as exc:
            logger.exception(exc)
            # 400: BAD_REQUEST
            api.abort(400, str(exc))
        else:
            player['uid'] = uid
            return {"data": player, "version": APP_VERSION}


@ns.route('/player/<string:uid>')
class PlayerEndpoint(BaseEndpoint):

    # pylint: disable=no-self-use
    @api.doc(description='Returns Player details.')
    @api.marshal_with(player_resp)
    def get(self, uid):
        player = redis_store.get("player:{}".format(uid))
        return {"data": player, "version": APP_VERSION}

    # pylint: disable=no-self-use
    @api.doc(description='Updates Player.', body=player_model)
    @api.marshal_with(player_resp)
    def patch(self, uid, **kwargs):
        self.validate_player(request.json)
        player = redis_store.get("player:{}".format(uid))
        if not player:
            api.abort("Player not found.")

        return self.save_player(uid, player)

    # pylint: disable=no-self-use
    @api.doc(description='Deletes Player.')
    def delete(self, uid):
        player = redis_store.get("player:{}".format(uid))
        if not player:
            api.abort("Player not found.")
        redis_store.delete("player:{}".format(uid))
        return {'data': player, 'version': APP_VERSION}


@ns.route('/players')
class PlayerListEndpoint(BaseEndpoint):
    """
    Leaderboard
    """
    # pylint: disable=no-self-use
    @api.doc(description='Players leaderboard.')
    def get(self, **kwargs):
        result = redis_store.zrevrange('scores', 0, 4, 'leaderboard')
        data = []
        for name, rank in result:
            data.append({"name": name.decode(), "rank": rank})
        all_players = []
        for key in redis_store.scan_iter("player:*"):
            player_data = json.loads(redis_store.get(key))
            player_data['id'] = key.decode().split(':')[1]
            all_players.append(player_data)
        return {'data': {'leaderboard': data, 'all_players': all_players}, 'version': APP_VERSION}

    # pylint: disable=no-self-use
    @api.doc(description='Updates leaderboard scores.', body=score_model)
    def patch(self, **kwargs):
        # Only staff user is allowed to update leaderboard
        user_role = kwargs.get('creds', {}).get('user_role', None)
        user_id = kwargs.get('creds', {}).get('user_id', None)
        if user_role != 'Staff' and user_id != uid:
            api.abort(403, "Access denied.")

        player_name = request.json.get('name')
        rank = request.json.get('rank')
        data = {player_name: float(rank)}
        redis_store.zadd('scores', data, 'leaderboard')
        return {'data': data, 'version': APP_VERSION}

    # pylint: disable=no-self-use
    @api.doc(description='Adds Player.', body=player_model)
    @api.marshal_with(player_resp)
    def post(self, **kwargs):
        self.validate_player(request.json)
        uid = str(uuid.uuid4())
        player = request.json
        return self.save_player(uid, player)
