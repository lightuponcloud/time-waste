import json
import logging
import sys
import requests

import test_battle.settings as settings

logger = logging.getLogger(__name__)

def move(player_id, token):
    """
    Calls the battleserver to make a move.
    """
    players_endpoint = 'http://battle_service:5000/v1/battle/battle/{}'.format(player_id)
    data = {
        'player_id': player_id,
        'action': 'attack'
    }
    response = requests.post(players_endpoint, headers={
        'authorization': settings.ADMIN_SECRET,
        'X-Subject-Token': settings.ADMIN_SECRET,
        'Content-Type': 'application/json;charset=UTF-8',
        'Accept': 'application/json'
    }, data=json.dumps(data))
    if response.status_code != 200:
        logger.error('Unexpected response from battle service.')
        raise Exception("Something's went wrong in async process.")
    json_data = response.json()
    logger.info('Response from battle service: {}'.format(json_data))
    return json_data.get('is_finished', False)


def process_battle(data, **kwargs):
    """
    In real application this function would probably start a websocket server,
    accept commands and forward them battle server.
    """
    data = json.loads(data)
    player1_id = data.get('player1_id')
    player2_id = data.get('player2_id')
    token = data.get('token')
    battle_finished = False
    cnt = 0
    while battle_finished is not True:
        if cnt % 2 == 0:
            action_player_id = player1_id
        else:
            action_player_id = player2_id
        battle_finished = move(action_player_id, token)
