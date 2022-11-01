import logging
from flask import request
from flask import g as flask_g

from test_auth.app import api, ns, db, limiter
from test_auth.resource import TestResource
from test_auth.api import default_parser

from test_auth.app import Recipe, Ingredient, RecipeIngredient
from .spec import ingredient_model, recipe_model, recipe_response_model

logger = logging.getLogger(__name__)  # pylint: disable=invalid-name


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

    def save_ingredients(self, ingredients):
        """
        Creates recipe ingredients records in persistent DB.
        """
        total_calories = 0
        all_ingredients = []
        try:
            for ingredient in ingredients:
                name = ingredient.get('name')
                calories = ingredient.get('calories_per_100_grams')
                if Ingredient.query.filter_by(name=name).count() == 0:
                    ingredient = Ingredient(name=name, value100gm=calories)
                    db.session.add(ingredient)
                else:
                    ingredient = Ingredient.query.filter_by(name=name).first()
                all_ingredients.append(ingredient)
                total_calories += calories
        except Exception as exc:
            logger.exception(exc)
            return False
        else:
            db.session.commit()
            return {'ingredients': all_ingredients, 'total_calories': total_calories}

    def save_recipe(self, name, intro, total_calories, ingredients):
        """
        Creates recipe and relation to ingredients in persistent DB.
        """
        try:
            recipe = Recipe(
                name=name,
                intro=intro,
                total_calories=total_calories
            )
            db.session.add(recipe)
        except Exception as exc:
            logger.exception(exc)
            return False
        else:
            db.session.commit()

        try:
            for ingredient in ingredients:
                recipe_ingredient = RecipeIngredient(recipe_id=recipe.id, ingredient_id=ingredient.id)
                db.session.add(recipe_ingredient)
        except Exception as exc:
            logger.exception(exc)
            return False
        else:
            db.session.commit()
            return recipe

@ns.route('/recipes/<string:uid>')
class RecipeEndpoint(BaseEndpoint):

    # pylint: disable=no-self-use
    @api.doc(description='Returns Recipe details.')
    @api.marshal_with(recipe_model)
    @limiter.limit("100 per minute")
    def get(self, uid):
        recipe = Recipe.query.filter_by(id=uid).first_or_404()
        return {
            'id': recipe.id,
            'name': recipe.name,
            'introduction': recipe.intro,
            'ingredients': [{
                    'name': i.name,
                    'calories_per_100_grams': i.value100gm
                    } for i in recipe.ingredients]
        }

    # pylint: disable=no-self-use
    @api.doc(description='Updates Recipe.', body=recipe_model)
    @api.marshal_with(recipe_model)
    @limiter.limit("100 per minute")
    def patch(self, uid):
        user = flask_g.session_user
        if not user:
            api.abort(httplib.FORBIDDEN, 'Not allowed to access this API endpoint.')

        recipe = Recipe.query.filter_by(id=uid).first_or_404()

        ingredients = request.json.get('ingredients', [])
        if ingredients:
            # remove previous recipe ingredients
            for recipe_ingredient in RecipeIngredient.query.filter_by(recipe_id=recipe.id).all():
                db.session.delete(recipe_ingredient)

            result = self.save_ingredients(request.json.get('ingredients', []))
            if not result:
                # 400: BAD_REQUEST
                api.abort(400, str(exc))
            else:
                try:
                    for ingredient in result['ingredients']:
                        recipe_ingredient = RecipeIngredient(recipe_id=recipe.id, ingredient_id=ingredient.id)
                        db.session.add(recipe_ingredient)
                except Exception as exc:
                    logger.exception(exc)
                    # 400: BAD_REQUEST
                    api.abort(400, str(exc))
                else:
                    db.session.commit()
            recipe.total_calories = result['total_calories']

        name = request.json.get('name', None)
        if name:
            recipe.name = name
        intro = request.json.get('introduction', None)
        if intro:
            recipe.intro = intro
        db.session.add(recipe)
        db.session.commit()
        return {
            'id': recipe.id,
            'name': recipe.name,
            'introduction': recipe.intro,
            'ingredients': [{
                    'name': i.name,
                    'calories_per_100_grams': i.value100gm
                    } for i in recipe.ingredients],
            'total_calories': recipe.total_calories
        }

    # pylint: disable=no-self-use
    @api.doc(description='Deletes Recipe.')
    @limiter.limit("100 per minute")
    def delete(self, uid):
        user = flask_g.session_user
        if not user:
            api.abort(httplib.FORBIDDEN, 'Not allowed to access this API endpoint.')

        recipe = Recipe.query.filter_by(id=uid).first_or_404()
        db.session.delete(recipe)
        db.session.commit()
        return {'data': []}


@ns.route('/recipes')
class RecipeListEndpoint(BaseEndpoint):
    """
    List of Recipes
    """
    # pylint: disable=no-self-use
    @api.doc(description='List of recipes.')
    @api.marshal_with(recipe_response_model)
    @limiter.limit("100 per minute")
    def get(self):
        recipes = Recipe.query.all()
        data = []
        for recipe in recipes:
            data.append({
                'id': recipe.id,
                'name': recipe.name,
                'introduction': recipe.intro,
                'ingredients': [{
                    'name': i.name,
                    'calories_per_100_grams': i.value100gm
                    } for i in recipe.ingredients],
                'total_calories': recipe.total_calories
            })
        return {'data': data, 'version': '0.1'}


    # pylint: disable=no-self-use
    @api.doc(description='Create Recipe',
             body=recipe_model,
             parser=default_parser)
    @api.marshal_with(recipe_model)
    @limiter.limit("100 per minute")
    def post(self):
        user = flask_g.session_user
        if not user:
            api.abort(httplib.FORBIDDEN, 'Not allowed to access this API endpoint.')

        result = self.save_ingredients(request.json.get('ingredients', []))
        if not result:
            # 400: BAD_REQUEST
            api.abort(400, "Error saving ingredients.")
        else:
            recipe = self.save_recipe(
                request.json.get('name'),
                request.json.get('introduction'),
                result['total_calories'],
                result['ingredients']
            )
            if not recipe:
                # 400: BAD_REQUEST
                api.abort(400, "Error saving recipe.")

        return {
            'id': recipe.id,
            'name': recipe.name,
            'introduction': recipe.intro,
            'ingredients': [{
                    'name': i.name,
                    'calories_per_100_grams': i.value100gm
                    } for i in recipe.ingredients],
            'total_calories': recipe.total_calories
        }
