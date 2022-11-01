# -*- coding: utf-8 -*-

"""
Display registered Flask routes
"""

# See http://flask.pocoo.org/snippets/117/

from urllib import parse

from flask import url_for
from test_auth.app import app
from prettytable import PrettyTable
from .cli import cli


_ROUTE_COLS = ['Name', 'Methods', 'Path']


def _list_routes():
    """Create list of Flask routes"""

    # SERVER_NAME must be overridden temporarily for url_for
    # to work correctly
    old_server_name = app.config.get('SERVER_NAME', None)
    app.config['SERVER_NAME'] = ''

    try:
        result = PrettyTable(_ROUTE_COLS)
        for col in _ROUTE_COLS:
            result.align[col] = 'l'
        rows = []

        with app.app_context():
            for rule in app.url_map.iter_rules():
                options = {}
                for arg in rule.arguments:
                    # TODO: Added to suport non-sting
                    # params in path - something better?
                    options[
                        arg] = 123 if 'code' in arg else "[{0}]".format(arg)
                methods = ','.join(rule.methods)
                options['_external'] = False
                url = url_for(rule.endpoint, **options)
                rows.append([rule.endpoint, methods, parse.unquote(url)])

        rows.sort()
        for row in rows:
            result.add_row(row)

        return str(result)
    finally:
        app.config['SERVER_NAME'] = old_server_name


@cli.command(help=__doc__)
def list_routes():
    """
    List routes defined in application.
    """
    print(_list_routes())
