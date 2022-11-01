#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
"""
Least recently used cache

Implement a least recently used (LRU) cache mechanism as a decorator and demonstrate it use in
a small script. The LRU must be able to admit a ‘max_size’ parameter that by default has to be
100.

Candidates are advised to consider how an LRU cache should work. One source of information
is: https://en.wikipedia.org/wiki/Cache_replacement_policies#LRU
"""
from functools import wraps
from collections import OrderedDict
import logging
import random

logging.basicConfig()
logger = logging.getLogger("cache_demo")
logger.setLevel(logging.DEBUG)


def lru_cache(max_size=100):
    """
    Least-recently-used cache decorator.

    This implementation is not thread-safe and has a room for optimization.
    """
    def wrap(func):
        if max_size is None:
            logger.debug("Initializing cache.")
            cache = dict()

            @wraps(func)
            def wrapper(*args, **kwds):
                key = args
                if kwds:
                    key += tuple(sorted(kwds.items()))
                try:
                    result = cache[key]
                    logger.debug("Cache hit.")
                    return result
                except KeyError:
                    pass
                result = func(*args, **kwds)
                logger.debug("Cache miss.")
                cache[key] = result
                return result
        else:
            cache = OrderedDict()

            @wraps(func)
            def wrapper(*args, **kwds):
                key = args
                if kwds:
                    key += tuple(sorted(kwds.items()))
                try:
                    result = cache[key]
                    logger.debug("Cache hit.")
                    return result
                except KeyError:
                    pass
                result = func(*args, **kwds)
                logger.debug("Cache miss.")
                cache[key] = result
                if len(cache) > max_size:
                    cache.popitem(0)
                return result
        return wrapper
    return wrap

@lru_cache(max_size=10)
def demo(* args, **kwargs):
    return random.randint(1, 10)


def main():
    demo(*[1,2])
    demo(*[1,2])
    demo(*[2,3])
    demo(*[2,3])
    demo()
    demo()
    demo()
    demo()
    demo()

if __name__ == '__main__':
    main()
