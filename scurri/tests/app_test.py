# -*- coding: utf-8 -*-

import logging
import os.path as op
import sys
import unittest

PROJECT_ROOT = op.dirname(op.abspath(__file__))
sys.path.insert(0, op.dirname(PROJECT_ROOT))

from print_numbers import process_number
from post_codes import strip_space, validate

# The following 100 codes were selected randomly from 1.7 million db file
TEST_VALID_CODES = ['KA3 7NQ', 'B70 6EY', 'RG248NL', 'CV3 2FU', 'DA1 9SN',
		    'BN108PD', 'ML6 0EE', 'E1W 2JR', 'BS161XR', 'UB7 9ED',
		    'NW8 6NA', 'DT9 3SF', 'RG316QU', 'HU170PZ', 'SE2 9WS', 
		    'NN3 6HB', 'TF3 5GL', 'TA248AT', 'CT155HY', 'PE133SS', 
		    'CM3 5RF', 'OL7 9HN', 'SO154LD', 'BL6 7AR', 'PE229QS', 
		    'RG278WX', 'LL297PS', 'BD133ED', 'CF466PG', 'S41 0NH', 
		    'CM232AL', 'NW3 7SU', 'E16 2NZ', 'GU7 3AD', 'W1S 4NJ', 
		    'PO317PY', 'IG2 7BB', 'NW1W7BZ', 'PO119DG', 'BR2 9RD', 
		    'LS167EA', 'BB7 1ET', 'PO2 9LY', 'WD197DE', 'SP6 2PH', 
		    'EH216BY', 'BS362QY', 'DH1 2ER', 'RH6 9FL', 'SY245BX', 
		    'CM5 0DY', 'AB355RG', 'TR108LW', 'DT101BG', 'DH1 1SZ', 
		    'ST129JA', 'AL6 0JX', 'SW170TX', 'DL1 2AP', 'EN5 5AY', 
		    'BA146EU', 'DG7 2EP', 'SE1P5WW', 'LD2 3BA', 'GU290LA', 
		    'HU7 3FX', 'TS9 7BJ', 'GL503EG', 'EN7 5AN', 'DL116PE', 
		    'SE9 4PG', 'GL2 4PU', 'LN6 7HZ', 'WD181SP', 'L36 4QB', 
		    'MK4 4AT', 'SG3 6XA', 'NW8 7LU', 'SY3 5DB', 'WN4 9HX', 
		    'KA137QA', 'B77 9LA', 'BH238BY', 'MK402EA', 'SR2 9NE', 
		    'PE191DR', 'S60 1HB', 'NG8 1PB', 'BA4 5HD', 'SE137NQ', 
		    'PL157QQ', 'TN4 8HT', 'BA160TA', 'DH1 4BD', 'KA107HH', 
		    'LN5 7AZ', 'LS131JL', 'BB111XD', 'NN5 7BF', 'CM111QL']

TEST_INCORRECT_CODES = [
    "AB1 1AA",  # Single digit district number is not allowed for AB area
    "BR12 1AA",  # Double digit district number is not allowed for BR area
    "BR0 1AA",  # zero district is not allowed for BR area
    "QR0 1AA",  # post code can't start with Q
    "AR0 1AC",  # final letter of post code can't be 'C'
]


class Testing(unittest.TestCase):

    def test_numbers(self):
        self.assertEqual(process_number(0), "0")
        self.assertEqual(process_number(3), "Three")
        self.assertEqual(process_number(5), "Five")
        self.assertEqual(process_number(90), "ThreeFive")

    def test_uk_post_codes(self):
        self.assertEqual(strip_space("SW1A 1AA"), "SW1A1AA")
        self.assertEqual(strip_space("SW1A1AA"), "SW1A1AA")
        self.assertEqual(strip_space("S W1A1AA"), "S W1A1AA")

        self.assertEqual(validate(None), False)
        self.assertEqual(validate('ABCDEFGHIJKLMNOP'), False)
        self.assertEqual(validate('AB'), False)

        for code in TEST_VALID_CODES:
            print("Checking {}".format(code))
            self.assertEqual(validate(code), True)

        for code in TEST_INCORRECT_CODES:
            print("Checking {}".format(code))
            self.assertEqual(validate(code), False)


if __name__ == "__main__":
    unittest.main()
