
log_parse.py is tested with Python 2.7.15 on a ~30 MB data set.

It accepts one argument, the name of file. For example:

% ./log_parse.py -f input

Output was the following

"""
Starting 2 threads
Thread finished processing in 6 seconds
Thread finished processing in 13 seconds
Log starts at Sep 22 15:38:21 and ends at Aug 03 22:14:15
Average length of message in log is 53 characters
Record Severity          Count
             47         150000
             13         150000
            165         150000
             34         150000
"""

First script performs such checks as file size and file type.
If file is less than 3 MB, it processes data in one thread.
Otherwise it spawns N workers, where N is the number of cores
available in system.

Each worker accepts file path and seek position. It starts reading log records from that position.
Once all workers finished processing data, application aggregates them and prints to console.


NB. multiprocessing module is gull of bugs, so in real life I would use framework like Twisted
with to spawn workers and collect results.
