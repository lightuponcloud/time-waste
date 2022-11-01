#!/usr/bin/env python
import argparse

import websocket
import json
import thread
import time

def on_message(ws, message):
    try:
        data = json.loads(message)
    except (ValueError, TypeError):
        pass
    else:
        if isinstance(data, dict):
            account_id = data.get('account_id')
            if ws.cookie and account_id != ws.cookie:
                print("Ignoring account.")
            else:
                print(message)

def on_error(ws, error):
    print(error)

def on_close(ws):
    print("### closed ###")

def on_open(ws):
    def run(*args):
        while True:
            time.sleep(1)
        ws.close()
        print("thread terminating...")
    thread.start_new_thread(run, ())

def create_parser():
    """
    The command line arguments parser
    :return: the parameters pass on the command line
    """
    parser = argparse.ArgumentParser(
        description='Utility to connect to websocket server')
    parser.add_argument('-a', type=str, help='Account filter. For example: -a "celtra"')
    parser.add_argument('-i', type=str, help='IP/Hostname. For example: -i 127.0.0.1', required=True)
    parser.add_argument('-p', type=int, help='Port. For example: -p 8082', required=True)
    args = parser.parse_args()
    return args

def main():
    parser = create_parser()
    host = parser.i
    port = parser.p
    account_filter = parser.a

    websocket.enableTrace(True)
    ws = websocket.WebSocketApp("ws://{}:{}/websocket".format(host, port),
                              on_message = on_message,
                              on_error = on_error,
                              on_close = on_close,
                              cookie=account_filter)
    ws.on_open = on_open
    ws.run_forever()

if __name__ == '__main__':
    main()
