from flask_restx import fields

from test_battle.app import api


battle_model = api.model('Battle', {
    'player1_id': fields.String(),
    'player2_id': fields.String(),
    'player1_health': fields.Integer(),
    'player2_health': fields.Integer(),
    'is_finished': fields.Boolean()
})

battle_log = api.model('BattleLog', {
    "player_id": fields.String(),
    "action": fields.String(),
    'player_attack_value': fields.Integer()
})
