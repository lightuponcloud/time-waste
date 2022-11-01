#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Task:
Write a library that supports validating and formatting post codes for UK. 
The details of which post codes are valid and which are the parts they 
consist of can be found at 
https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom#Formatting.

Analysis:
Real world application would check UK postal code against the database,
as there'a inactive codes. I assume the objective of this application
is to wirte neat code and cover it with unit test cases, in order 
to demonstrate ability to understand system requirements specification.


Usage example:
./post_codes.py -c SW1A1AA
./post_codes.py -c "SW1A 1AA"

"""
import argparse
import sys
import re

POST_CODE_RE = re.compile("^(GIR ?0AA|[A-PR-UWYZ]([0-9]{1,2}|([A-HK-Y][0-9]([0-9ABEHMNPRV-Y])?)|[0-9][A-HJKPS-UW]) ?[0-9][ABD-HJLNP-UW-Z]{2})$")

SINGLE_DIGIT_DISTRICTS = ["BR", "FY", "HA", "HD", "HG", "HR", "HS", "HX", "JE", "LD", "SM", "SR", "WC", "WN", "ZE"]

DOUBLE_DIGIT_DISTRICTS = ["AB", "LL", "SO"]

ZERO_DISTRICTS = ["BL", "BS", "CM", "CR", "FY", "HA", "PR", "SL", "SS"]


def strip_space(post_code):
    """
    The space, if present, should be before the final three characters.
    """
    if post_code[-4] == ' ':
        # Strip space to not make RegEx even less comprehensible
        outward_code = post_code[:-4]
        inward_code = post_code[-3:]
        return "{}{}".format(outward_code, inward_code)
    return post_code


def validate(post_code):
    """
    Validates post code by checking it against rules,
    defined by the following link

    https://en.wikipedia.org/wiki/Postcodes_in_the_United_Kingdom#Formatting
    """
    # Perform some preliminary checks in order to keep RegEx simple
    if not isinstance(post_code, str):
        return False
    length = len(post_code)
    if length < 6 or length > 8:
        return False

    post_code = strip_space(post_code)
    # If ther's still space character, consider post code as incorrect,
    # as space can be used only before last 3 characters
    if ' ' in post_code:
        return False

    area_code = post_code[:-3]
    if area_code[:2] in SINGLE_DIGIT_DISTRICTS:
        # single-digit districts
        found_digits = re.findall(r'[0-9]+', area_code)
        if len(found_digits) != 1:
            return False
        if len(found_digits[0]) != 1:
            return False
    elif area_code[:2] in DOUBLE_DIGIT_DISTRICTS:
        # single-digit districts
        found_digits = re.findall(r'[0-9]+', area_code)
        if len(found_digits) != 1:
            return False
        if len(found_digits[0]) != 2:
            return False

    if area_code[:2] not in ZERO_DISTRICTS:
        found_digits = re.findall(r'[0-9]+', area_code)
        if int(found_digits[0]) == 0:
            # Only limited districts have zero code
            return False
    if area_code[0].upper() in ['Q', 'V', 'X']:
        # The letters QVX are not used in the first position.
        return False

    if post_code[-1].upper() in ['C', 'I', 'K', 'M', 'O', 'V']:
        # The final two letters do not use the letters CIKMOV, 
        # so as not to resemble digits or each other when hand-written.
        return False

    result = POST_CODE_RE.match(post_code)
    if result and result.groups():
        return True
    return False


def create_parser():
    """
    The command line arguments parser
    :return: the parameters pass on the command line
    """
    parser = argparse.ArgumentParser(description='Test assignment.')
    parser.add_argument('-c', type=str, help='UK postal code.', required=True)
    args = parser.parse_args()
    return args


def main():
    parser = create_parser()
    post_code = parser.c

    result = validate(post_code)
    if result is False:
        print("Non-existing post code.")
        sys.exit(-1)
    print("This is a valid UK post code.")
    sys.exit(0)

if __name__ == "__main__":
    main()
