import random
import logging
import uuid
import json
import requests
import redis
from rq import Queue, Connection

from flask import request
from flask_restx import fields

from test_battle.app import api, ns, redis_store, APP_VERSION
from test_battle.resource import TestResource
from test_battle.utils import json_dump, round as decimal_round
from test_battle.workers import process_battle

import test_battle.settings as settings

from .spec import battle_model, battle_log

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name

#
# The following response model will allow to tag responses with versions of API
#
battle_resp = api.model('BattleResp', {
    'data': fields.Nested(battle_model),
    'version': fields.String(),
})

battle_log_resp = api.model('BattleLogResp', {
    'data': fields.Nested(battle_log),
    'version': fields.String(),
})

battle_action = api.model('BattleAction', {
    "player_id": fields.String(),
    "action": fields.String()
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


def update_leaderboard(player_name, resources_taken):
    endpoint = settings.LEADERBOARD_ENDPOINT
    data = {
        'name': player_name,
        'rank': resources_taken
    }
    response = requests.patch(endpoint, headers={
        'authorization': settings.ADMIN_SECRET,
        'X-Subject-Token': settings.ADMIN_SECRET,
        'Content-Type': 'application/json;charset=UTF-8',
        'Accept': 'application/json'
    }, data=json.dumps(data))
    if response.status_code != 200:
        api.abort(400, 'Unexpected response from authentication service.')


@ns.route('/battle/<string:uid>')
class BattleEndpoint(BaseEndpoint):

    # pylint: disable=no-self-use
    @api.doc(description='Returns Battle details.')
    @api.marshal_with(battle_resp)
    def get(self, uid, **kwargs):
        battle = redis_store.get("battle:{}".format(uid))
        if not battle:
            api.abort(404, "Battle not found.")
        battle = json.loads(battle)
        return {"data": battle, "version": APP_VERSION}

    # pylint: disable=no-self-use
    @api.doc(description='Add Battle Action.', body=battle_action)
    @api.marshal_with(battle_log_resp)
    def post(self, uid, **kwargs):
        """
        This API endpoint allows player to strike.
        """
        # uid is the player id ( battles are registered by player ids for ease of lookup )
        battle = redis_store.get("battle:{}".format(uid))
        if not battle:
            api.abort(400, "Battle has finished.")
        battle = json.loads(battle)
        if battle.get('is_finished'):
            api.abort(400, "Battle has finished.")

        # Check if the first move is performed by player who has started the battle
        battle_id = battle.get('battle_id')
        log_length = redis_store.llen("battle_log:{}".format(battle_id))
        action_player_id = request.json.get('player_id')
        battle_player1_id = battle.get('player1_id')
        if log_length > 0:
            if action_player_id != battle_player1_id:
                api.abort(400, 'The player who started battle strikes first.')
            # Check if the last move belongs to opponent
            last_action = redis_store.lindex("battle_log:{}".format(battle_id), 0)
            last_action_data = json.loads(last_action)
            last_action_player_id = last_action_data.get('player_id')
            if last_action_player_id == action_player_id:
                api.abort(400, 'Waiting for the opponent move.')

        # get player's info: hit points, etc.
        token = kwargs.get('token')
        players_endpoint = settings.PLAYERS_ENDPOINT.format(uid)
        response = requests.get(players_endpoint, headers={
            'authorization': token,
            'X-Subject-Token': token,
            'Content-Type': 'application/json;charset=UTF-8',
            'Accept': 'application/json'
        })
        if response.status_code != 200:
            api.abort(400, 'Unexpected response from authentication service.')
        json_data = response.json()
        attack_value = json_data.get('attack_value')
        # hit_points = json_data.get('hit_points')
        attack_player_name = json_data.get('name')
        attack_player_gold = json_data.get('gold')
        attack_player_silver = json_data.get('silver')

        # calculate the attach value
        battle_player2_id = battle.get('player2_id')
        health = None
        defender_uid = None
        if uid == battle_player2_id:
            # second player strikes
            health = battle.get('player2_health')
            defender_uid = battle.get('player1_id')
        else:
            health = battle.get('player1_health')
            defender_uid = battle.get('player2_id')

        health_reduced_percent = 100 - health
        if health_reduced_percent > 0:
            new_attack_value = (float(attack_value)/100)*health_reduced_percent
            # the attack can never be reduced below 50% of the base attack value
            if new_attack_value > attack_value / 2:
                attack_value = decimal_round(new_attack_value, places=0)

        # Use the luck value of the defender to decide if an attack misses.
        players_endpoint = settings.PLAYERS_ENDPOINT.format(defender_uid)
        response = requests.get(players_endpoint, headers={
            'authorization': token,
            'X-Subject-Token': token,
            'Content-Type': 'application/json;charset=UTF-8',
            'Accept': 'application/json'
        })
        if response.status_code != 200:
            api.abort(400, 'Unexpected response from authentication service.')
        json_data = response.json()
        luck_value = json_data.get('luck_value')
        defender_player_gold = json_data.get('gold')
        defender_player_silver = json_data.get('silver')
        damage = attack_value
        if random.uniform(0, 100) < luck_value:
            damage = 0

        # write action log record
        battle_log_record = {
            "player_id": uid,
            "action": "attack",
            'player_attack_value': attack_value
        }
        redis_store.lpush("battle_log:{}".format(battle_id), json_dump(battle_log_record))

        # The specification says the following:
        #
        # The battle continues until one of the players hit points value reaches zero.
        #
        # But health value is mentioned in 1.a.2.1 example. So I will use halth value instead

        if uid == battle_player2_id:
            # second player strikes
            player1_health = battle.get('player1_health')
            player1_health -= damage
            battle['player1_health'] = player1_health
        else:
            player2_health = battle.get('player2_health')
            player2_health -= damage
            battle['player2_health'] = player2_health

        if battle['player1_health'] <= 0 or battle['player2_health'] <= 0:
            # battle is finished
            steal_resources_percent = random.randint(10, 20)
            take_gold_percent = steal_resources_percent / 2
            take_silver_percent = steal_resources_percent / 2

            gold_taken = (float(defender_player_gold)/100)*take_gold_percent
            new_gold_value = defender_player_gold - gold_taken
            silver_taken = (float(defender_player_silver)/100)*take_silver_percent
            new_silver_value = defender_player_silver - silver_taken

            defender_data = json.dumps({
                'gold': new_gold_value,
                'silver': new_silver_value
            })
            # take resources from defender
            # only admin should be allowed to update players
            players_endpoint = settings.PLAYERS_ENDPOINT.format(defender_uid)
            response = requests.patch(players_endpoint, headers={
                'authorization': settings.ADMIN_SECRET,
                'X-Subject-Token': settings.ADMIN_SECRET,
                'Content-Type': 'application/json;charset=UTF-8',
                'Accept': 'application/json'
            }, data=defender_data)
            if response.status_code != 200:
                api.abort(400, 'Unexpected response from authentication service.')

            winner_data = json.dumps({
                'gold': attack_player_gold + gold_taken,
                'silver': attack_player_silver + silver_taken
            })
            # assign them to winner
            # only admin should be allowed to update players
            players_endpoint = settings.PLAYERS_ENDPOINT.format(uid)
            response = requests.patch(players_endpoint, headers={
                'authorization': settings.ADMIN_SECRET,
                'X-Subject-Token': settings.ADMIN_SECRET,
                'Content-Type': 'application/json;charset=UTF-8',
                'Accept': 'application/json'
            }, data=winner_data)
            if response.status_code != 200:
                api.abort(400, 'Unexpected response from authentication service.')

            update_leaderboard(attack_player_name, gold_taken+silver_taken)
        redis_store.set("battle:{}".format(uid), json_dump(battle))
        return {'data': battle, 'version': APP_VERSION}


@ns.route('/battles')
class BattleListEndpoint(BaseEndpoint):
    """
    Allows to start a new battle
    """
    # pylint: disable=no-self-use
    @api.doc(description='List of battles.')
    def get(self, **kwargs):
        data = []
        for key in redis_store.scan_iter("battle:*"):
            record = redis_store.get(key)
            battle = json.loads(record)
            data.append(battle)
        return {'data': data, 'version': APP_VERSION}

    # pylint: disable=no-self-use
    @api.doc(description='Starts a new battle.', body=battle_model)
    def post(self, **kwargs):
        player1_id = request.json.get('player1_id')
        player2_id = request.json.get('player2_id')

        is_player1_in_battle = False
        for key in redis_store.scan_iter("battle_{}:*".format(player1_id)):
            data = json.loads(redis_store.get(key))
            if not data.get('is_finished'):
                is_player1_in_battle = True
                break

        if is_player1_in_battle:
            api.abort(400, "Player1 is in battle now.")

        is_player2_in_battle = False
        for key in redis_store.scan_iter("battle_{}:*".format(player2_id)):
            data = json.loads(redis_store.get(key))
            if not data.get('is_finished'):
                is_player2_in_battle = True
                break

        if is_player2_in_battle:
            api.abort(400, "Player2 is in battle now.")

        battle_id = str(uuid.uuid4())
        battle_data = {
            'battle_id': battle_id,
            'player1_id': player1_id,
            'player2_id': player2_id,
            'player1_health': 100,
            'player2_health': 100,
            'is_finished': False
        }
        try:
            redis_store.set("battle:{}".format(player1_id), json_dump(battle_data))
            redis_store.set("battle:{}".format(player2_id), json_dump(battle_data))
        except Exception as exc:
            logger.exception(exc)
            # 400: BAD_REQUEST
            api.abort(400, str(exc))

        task_id = None
        with Connection(redis.from_url(settings.REDIS_URL)):
            q = Queue()
            task = q.enqueue(process_battle, json.dumps({
                'player1_id': player1_id,
                'player2_id': player2_id,
                'token': kwargs.get('token')
            }), timeout=200000, result_ttl=200000)  # 20 minutes
            task_id = task.get_id()
            logger.info("A new battle processing task has been created: {}".format(task_id))
        return {"data": battle_data, 'version': APP_VERSION}
