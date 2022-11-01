#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
import unittest

import anagram


class Test_Anagram(unittest.TestCase):

    def test_get_next_draw_date(self):
        result = anagram.anagram('abc', ['abc', '123'])
        self.assertEqual(set(result), set(['abc', 'acb', 'bac', 'bca', 'cab', 'cba']))

        result = anagram.anagram('abc', [])
        self.assertEqual(result, [])


if __name__ == '__main__':
    unittest.main()
