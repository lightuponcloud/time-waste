#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
This is a program that processes syslog messages from a file and
outputs useful statistics.
"""
import os
import os.path
import sys
import argparse
import signal
from multiprocessing import Pool, cpu_count
from datetime import datetime
from time import time


# According to https://www.ietf.org/rfc/rfc3164.txt,
# maximum log record length is 1024 octets
MAX_LOG_RECORD_SIZE = 1024


def create_parser():
    """
    The command line arguments parser
    :return: the parameters pass on the command line
    """
    parser = argparse.ArgumentParser(
        description='Utility for analyzing log file in old BSD format')
    parser.add_argument('-f', type=str, help='path to file')
    args = parser.parse_args()
    return args


def parse_line(line):
    """
    Example of input:
    <13>Oct  7 10:09:00 unicorn sched# invalid operation

    Returns severity, datetime object and length of log record itself
    """
    # regular expressions work much slower, so I parse log record using Python
    if not line.startswith('<'):
        return {'svr': None, 'len': 0, 'dt': None}
    bits = line.split('>', 1)
    if len(bits) !=2:
        return
    severity = bits[0][1:]
    date_bits = bits[1].split(None, 3)
    if len(date_bits) != 4:
        return
    month = date_bits[0]
    day = date_bits[1]
    time = date_bits[2]
    rest = date_bits[3]
    return {
        'svr': severity,
        'len': len(rest),
        'dt': datetime.strptime('%s %s %s %s'%(month, day, datetime.now().year, time),
                                '%b %d %Y %H:%M:%S')
    }

def process(file_path, seek_index=None):
    """
    Iterates over lines in ``file_path`` and returns the following
        - first timestamp
        - last timestamp
        - map of severity and count
        - all message lengths
    """
    t1 = time()
    result = dict(
        first_timestamp = None,
        last_timestamp = None,
        severity_cnt_map = {},
        all_message_lengths = []
    )
    with open(file_path, 'rb') as fd:
        if seek_index is not None:
            fd.seek(seek_index)
        for line in fd:
            # Python automatically uses buffered I/O and memory management
            # so I do not have to worry about large chunks
            record = parse_line(line)
            result['all_message_lengths'].append(record['len'])
            if record['svr'] in result['severity_cnt_map']:
                result['severity_cnt_map'][record['svr']] += 1
            else:
                result['severity_cnt_map'][record['svr']] = 1
            if not result['first_timestamp']:
                result['first_timestamp'] = record['dt']
            result['last_timestamp'] = record['dt']
    t2 = time()
    print "Thread finished processing in %d seconds" % int(t2-t1)
    return result


def analyze(file_path, file_size):
    result = {
        'avg_len': 0,
        'first_timestamp': None,
        'last_timestamp': None,
        'severity_cnt_map': {}
    }
    if file_size == 0:
        return result
    all_message_lengths = []
    severity_cnt_map = {}
    if file_size < 3145728:
        if file_size < MAX_LOG_RECORD_SIZE+2:
            seek_index = 0
        else:
            seek_index = file_size-MAX_LOG_RECORD_SIZE+2
        # single thread processing for small files
        result = process(file_path, seek_index=seek_index)
        if result['all_message_lengths']:
            avg_len = sum(result['all_message_lengths']) / len(result['all_message_lengths'])
            del result['all_message_lengths']
            result['avg_len'] = avg_len
            return result
        else:
            return result

    # if size of log file is bigger than 3 MB,
    # use seek and multiprocessing in order to keep RAM/CPU usage predictable
    pool_size = cpu_count()
    if pool_size >= 16:
        # usage of more than 16 threads is ineffective, as studies have shown
        pool_size = 16
    # Apart from other bugs, Python's multiprocessing module hangs on KeyboardInterrupt
    sigint_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
    pool = Pool(processes=pool_size)
    signal.signal(signal.SIGINT, sigint_handler)
    step = file_size / pool_size
    jobs = []
    print "Starting %d threads " % pool_size
    try:
        for x in xrange(pool_size):
            jobs.append( pool.apply_async(process, (file_path, x*step)) )
    except KeyboardInterrupt:
        sys.stderr.write("\nTerminated by user\n")
        pool.terminate()
        sys.exit(-1)

    all_lengths = []
    for job in jobs:
        job_result = job.get()
        if job_result['all_message_lengths']:
            all_lengths.extend(job_result['all_message_lengths'])
            if not result['first_timestamp']:
                result['first_timestamp'] = job_result['first_timestamp']
            else:
                if job_result['first_timestamp'] < result['first_timestamp']:
                    result['first_timestamp'] = job_result['first_timestamp']
            if not result['last_timestamp']:
                result['last_timestamp'] = job_result['last_timestamp']
            else:
                if job_result['last_timestamp'] > result['last_timestamp']:
                    result['last_timestamp'] = job_result['last_timestamp']
            for k in job_result['severity_cnt_map'].keys():
                if k not in result['severity_cnt_map']:
                    result['severity_cnt_map'][k] = job_result['severity_cnt_map'][k]
                else:
                    result['severity_cnt_map'][k] += job_result['severity_cnt_map'][k]

    pool.close()
    avg_len = sum(all_lengths) / len(all_lengths)
    result['avg_len'] = avg_len
    return result


def main():
    parser = create_parser()
    file_path = parser.f
    if not file_path:
        sys.stderr.write("\nFile not specified\n")
        sys.exit(-1)
    try:
        stat = os.stat(file_path)
    except OSError:
        sys.stderr.write("\nFile not found\n")
        sys.exit(-1)
    if not os.path.isfile(file_path):
        sys.stderr.write("\nNot a regular file\n")
        sys.exit(-1)

    result = analyze(file_path, stat.st_size)
    print "Log starts at %s and ends at %s" % (
        result['first_timestamp'].strftime("%b %d %H:%M:%S"),
        result['last_timestamp'].strftime("%b %d %H:%M:%S"))
    print "Average length of message in log is %d characters " % result['avg_len']

    severity_keys = result['severity_cnt_map'].keys()
    row_format ="{:>15}" * 2
    print row_format.format("Record Severity", "Count")
    for k in severity_keys:
        print row_format.format(k, result['severity_cnt_map'][k])


if __name__ == '__main__':
    main()
