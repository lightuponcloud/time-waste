from pbkdf2 import crypt
import re
from datetime import datetime, date
import decimal
import json

from werkzeug.security import safe_str_cmp


email_re = re.compile(
    r"(^[-!#$%&'*+/=?^_`{}|~0-9A-Z]+(\.[-!#$%&'*+/=?^_`{}|~0-9A-Z]+)*"  # dot-atom
    # quoted-string, see also http://tools.ietf.org/html/rfc2822#section-3.2.5
    r'|^"([\001-\010\013\014\016-\037!#-\[\]-\177]|\\[\001-\011\013\014\016-\177])*"'
    r')@((?:[A-Z0-9](?:[A-Z0-9-]{0,61}[A-Z0-9])?\.)+[A-Z]{2,6}\.?$)'  # domain
    r'|\[(25[0-5]|2[0-4]\d|[0-1]?\d?\d)(\.(25[0-5]|2[0-4]\d|[0-1]?\d?\d)){3}\]$', re.IGNORECASE)


class Pbkdf2(object):
    def __init__(self, app=None):
        if app is not None:
            self.app = app

    def check_password(self, password, encoded):
        pwhash = crypt(password, encoded)
        return safe_str_cmp(pwhash, encoded)

    def make_password(self, password):
        return crypt(password)


def is_email(addr):
    """
    Checks if valid email provided.
    """
    if not email_re.search(addr):
        return False
    return True


def _default_json(obj):
    """
    JSON serializer for objects not serializable by default json code
    """
    if isinstance(obj, decimal.Decimal):
        return (str(o) for o in [obj])
    elif isinstance(obj, (datetime, date)):
        return obj.isoformat()
    elif isinstance(obj, bytes):
        return obj.decode()


def json_dump(data):
    """
    Dumps data to JSON with custom serializer and ident.
    """
    return json.dumps(data, indent=4, default=_default_json)
