#!/usr/bin/env python2.7
import unittest
from datetime import datetime, timedelta

import date_cal


class Test_Draws(unittest.TestCase):

    def test_get_next_draw_date(self):
        today = datetime.strptime('08/11/2018', '%d/%m/%Y')

        result = date_cal.get_next_draw_date(today)
        self.assertEqual(result.day, 10)

        result = date_cal.get_next_draw_date(today+timedelta(days=1))
        self.assertEqual(result.day, 10)

        result = date_cal.get_next_draw_date(today+timedelta(days=2))
        self.assertEqual(result.day, 10)

        # test with time now
        time = datetime.strptime("20:00", "%H:%M")
        time = datetime.time(time)
        today = datetime.combine(today+timedelta(days=2), time)
        result = date_cal.get_next_draw_date(today)
        self.assertEqual(result.day, 14)

        today = datetime.strptime('11/11/2018', '%d/%m/%Y')
        result = date_cal.get_next_draw_date(today)
        self.assertEqual(result.day, 14)

        result = date_cal.get_next_draw_date(today+timedelta(days=1))
        self.assertEqual(result.day, 14)

        result = date_cal.get_next_draw_date(today+timedelta(days=2))
        self.assertEqual(result.day, 14)

        result = date_cal.get_next_draw_date(today+timedelta(days=3))
        self.assertEqual(result.day, 14)

        result = date_cal.get_next_draw_date(today+timedelta(days=4))
        self.assertEqual(result.day, 17)


if __name__ == '__main__':
    unittest.main()
