from decimal import Decimal, ROUND_UP, InvalidOperation, getcontext
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


class RoundedDecimalError:
    """
    General Purpose Error Handling used to handle error exceptions
    created in caller.
    Caller name and module taken from
    Activestate Recipe 66062: Determining Current Function Name
        # sys._getframe().f_code.co_name
        # sys._getframe().f_lineno
        # sys._getframe().f_code.co_filename
    """

    def __repr__(self):
        return "RoundedDecimalError - Partial Unit Rounding Error Exception Occured"

    def __init__(self, val='', id='', msg=''):
        import sys
        self.val = val
        self.id = id
        self.msg = msg
        self.caller_name = sys._getframe(1).f_code.co_name              #callers name
        self.caller_module = sys._getframe(1).f_code.co_filename        #module name of caller
        self.caller_lineno = sys._getframe(1).f_lineno          #line number of caller


def round_decimal(val='0', places=None, roundfactor='0', normalize=True):
    """
    PARTIAL UNIT ROUNDING DECIMAL
    Converts a valid float, integer, or string to a decimal number with a
    specified number of decimal places, performs "partial unit rounding",
    and decimal normalization.

    METHOD ARGUEMENTS:
    `val` The value to be converted and optionally formated to decimal.
    `places` The decimal precision is defined by integer "places" and
        must be <= the precision defined in the decimal.Decimal context.
    `roundfactor` (partial unit rounding factor) If purf is between -1
        and 1, purf rounds up (positive purf value) or down (negative
        purf value) in factional "purf" increments.
    `normalize` If normalize is True (any value other than False),
        then rightmost zeros are truncated.

    """

    #-- Edit function arguments and set necessary defaults

    # Allow templates to submit False from filter
    if str(normalize) == 'False': normalize = False

    try:
        roundfactor = Decimal(str(roundfactor))
    except (InvalidOperation, ValueError):
        raise RoundedDecimalError(val=roundfactor, id =1, msg="roundfactor - InvalidOperation or ValueError")  #reraise exception and return to caller
    if not (abs(roundfactor) >= 0 and abs(roundfactor) <= 1):
        raise RoundedDecimalError(val=roundfactor, id=2, msg="roundfactor - Out of Range - must be between -1 and +1")
    try:
        if places != None: places = int(places)
    except ValueError:
        raise RoundedDecimalError(val=places, id=3, msg='ValueError, Invalid Integer ')
    if places > getcontext().prec:
        raise RoundedDecimalError(val=places, id=4, msg='places Exceeds Decimal Context Precision')
    try:
        decval = Decimal(str(val))
    except (InvalidOperation, UnicodeEncodeError, TypeError):
        raise RoundedDecimalError(val=val, id=5, msg='InvalidOperation - val cannot be converted to Decimal')

    #-- Round decimal number by the Partial Unit Rounding Factor
    if roundfactor and decval%roundfactor:
        if roundfactor < 0: roundby = 0
        else: roundby = (decval/abs(decval))*roundfactor        #change sign of roudby to decval
        decval=(decval//roundfactor*roundfactor)+roundby #round up or down by next roundfactor increment

    #-- Adjust number of decimal places if caller provided decimal places
    if places != None:
        decmask = '0.'.ljust(places+2,'0') #i.e. => '.00' if places eq 2
        decval=decval.quantize(Decimal(decmask), rounding=ROUND_UP)  #convert to Decimal and truncate to two decimals

    #-- normalize - strips the rightmost zeros... i.e. 2.0 => returns as 2
    if normalize:
        # if the number has no decimal portion return just the number with no decimal places
        # if the number has decimal places (i.e. 3.20), normalize the number (to 3.2)
        # This check is necesary because normalizing a number which trails in zeros (i.e. 200 or 200.00) normalizes to
        # scientific notation (2E+2)
        if decval==decval.to_integral():
            decval = decval.quantize(Decimal('1'))
        else:
            decval.normalize()

    return decval


def round(value, places=2):
    return Decimal(round_decimal(value, places=places, normalize=False))
