#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
import unittest
import cache_demo


class Test_Decorators(unittest.TestCase):

    def test_decorator_cache_demo(self):
        class LRU_Test(object):
            def __init__(self):
                self.num = 0

            @cache_demo.lru_cache(max_size = 10)
            def test_method(self, num):
                self.num += num
                return self.num

        @cache_demo.lru_cache(max_size = 10)
        def test_func(num):
            return num

        c1 = LRU_Test()
        c2 = LRU_Test()
        m1 = c1.test_method
        m2 = c2.test_method
        f1 = test_func

        self.assertEqual(m1(1), m1(1)) 
        self.assertEqual(c1.num, 1)
        self.assertEqual(f1(1), f1(1))

        c1.num = 0
        c2.num = 10
        self.assertEqual((f1(0), m1(0), m2(0)), (0, 0, 10))
        self.assertEqual((f1(0), m1(0), m2(0)), (0, 0, 10))
        self.assertEqual((f1(1), m1(1), m2(1)), (1, 1, 11))

if __name__ == '__main__':
    unittest.main()
