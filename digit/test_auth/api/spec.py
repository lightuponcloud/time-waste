from flask_restx import fields

from test_auth.app import api


player_model = api.model('Player', {
    'name': fields.String(),
    'description': fields.String(),
    'gold': fields.Integer(),
    'silver': fields.Integer(),
    'attack_value': fields.Integer(),
    'hit_points': fields.Integer(),
    'luck_value': fields.Integer()
})

score_model = api.model('Score', {
    'name': fields.String(),
    'rank': fields.Integer()
})
