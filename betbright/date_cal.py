#!/usr/bin/env python2.7
"""
Date Calculation

The Irish lottery draw takes place twice weekly on a Wednesday
and a Saturday at 8pm. Write a function that calculates and
returns the next valid draw date based on an optional supplied
datetime parameter. If no supplied date is provided, assume current date time.
"""
from datetime import datetime, timedelta
import logging
import calendar
import argparse
import sys

logging.basicConfig()
logger = logging.getLogger("date_calculator")  # pylint: disable=invalid-name


def create_parser():
    """
    The command line arguments parser
    :return: the parameters pass on the command line
    """
    parser = argparse.ArgumentParser(description='Test assignment.')
    parser.add_argument('-d', type=str, help='Date in format dd/mm/YYYY.')
    parser.add_argument('-t', type=str, help='Time in format HH:MM.')
    args = parser.parse_args()
    return args


def get_next_draw_date(now):
    """
    Returns the next lottery draw date.
    """
    week_day = calendar.weekday(now.year, now.month, now.day)
    if week_day in [2,5]:  # Wednesday or Saturday
        if now.hour < 20:
            return now  # you will get in time
        else:
            return get_next_draw_date(now + timedelta(days=1))
    if week_day in [0,1]:  # Monday, Tuesday
        return now - timedelta(days=week_day) + timedelta(days=2)
    if week_day == 6:  # Sunday
        return now + timedelta(days=3)
    return now - timedelta(days=week_day) + timedelta(days=5)


def main():
    parser = create_parser()
    date = None
    time = None
    if parser.d:
        try:
            date = datetime.strptime(parser.d, "%d/%m/%Y")
        except (TypeError, ValueError):
            logger.warn("Failed to parse date \"{}\".".format(parser.d))
    if date is None:
        date = datetime.now()
    if parser.t:
        try:
            time = datetime.strptime(parser.t, "%H:%M")
        except (TypeError, ValueError):
            logger.warn("Failed to parse time \"{}\".".format(parser.t))
        else:
            time = datetime.time(time)
            date = datetime.combine(date, time)
    print "Next draw date: {}".format(
        get_next_draw_date(date).strftime("%d/%m/%Y ( 20:00 )"))
    sys.exit(0)

if __name__ == '__main__':
    main()
