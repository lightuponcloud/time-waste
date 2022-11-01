#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
"""
Find the anagram

Write a function that accepts a word (string) and a list of words (list or tuple of strings) and return
back a list with the valid anagrams for the word inside the given words list.

Usage example: python2 anagram.py -w blah -l "blah, something"
"""
from itertools import permutations
import argparse
import sys


def create_parser():
    """
    The command line arguments parser
    :return: the parameters pass on the command line
    """
    parser = argparse.ArgumentParser(description='Test assignment.')
    parser.add_argument('-w', type=str, help='Word to generate anagrams for.')
    parser.add_argument('-l', type=str, help='Comma-separated list of words to filter by.')
    args = parser.parse_args()
    return args


def anagram(word, lst):
    if not word or not lst:
        return []
    result = []
    prepared_list = [i.strip() for i in lst]
    if word.strip() in prepared_list:
        for item in permutations(word):
            result.append(u"".join(item))
    return result

def main():
    parser = create_parser()
    word = parser.w
    lst = parser.l
    if lst:
        lst = lst.split(',')
    result = anagram(word, lst)
    for item in result:
        print item
    sys.exit(0)

if __name__ == '__main__':
    main()
