from flask_restplus import fields

from test_auth.app import api


ingredient_model = api.model('Ingredient', {
    'name': fields.String(),
    'calories_per_100_grams': fields.Integer(),
})

recipe_model = api.model('Recipe', {
    'id': fields.Integer(),
    'name': fields.String(),
    'introduction': fields.String(),
    'ingredients': fields.List(fields.Nested(ingredient_model))
})

#
# Version attribute allows to use versioning of database as well
#
recipe_response_model = api.model('RecipesList', {
    'version': fields.String(),
    'data': fields.List(fields.Nested(recipe_model))
})
