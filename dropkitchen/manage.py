#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
Entry point for Test auth management commands
"""


from test_auth.management.cli import cli

import authapp  # NOQA


if __name__ == '__main__':
    cli()
