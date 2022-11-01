Celtra backend programming assignment
=====================================

Server Requirements
===================
* Erlang >= 19

Client Requirements

* python-websockets


Usage example
=============
1. Start web servier:

    ./run.sh

    That command builds the project, creates mnesia database and launches web server.

2. Connect clients ( aka "services" ):

     ./client.py -i 127.0.0.1 -p 8082 -a celtra

     "-a" is an account filter, that is optional

3. Send tracking info:

curl -vv -k -X POST "http://127.0.0.1:8082/tracking/celtra" \
    -H "accept: application/json" \
    -H "Content-Type: application/json" \
    -d "{ \"data\": \"0d599f0ec05c3bda8c3b8a68c32a1b47\" }"


You should see the following on the client side.

--- response header ---
HTTP/1.1 101 Switching Protocols
connection: Upgrade
date: Thu, 27 Sep 2018 14:01:23 GMT
sec-websocket-accept: eNhToF0OJuRvLbqq0SHxcaQjwOk=
server: Cowboy
upgrade: websocket
-----------------------
{"account_id":"celtra","timestamp":1538056370,"data":"0d599f0ec05c3bda8c3b8a68c32a1b47"}


Ther'a two accounts: "celtra", which is active and "test", which is inactive

The following command should return:
{"error":"Account is inactive"}

curl -vv -k -X POST "http://127.0.0.1:8082/tracking/test" \
    -H "accept: application/json" \
    -H "Content-Type: application/json" \
    -d "{ \"data\": \"0d599f0ec05c3bda8c3b8a68c32a1b47\" }"


How it works
============

Web application receives JSON request and sends message to the broadcast server:
    events_server:send_message(Msg)

"events_server" is a gen_server, that runs in separate Erlang process and do the following.

1. It broadcasts received message to all subscribers. In this case subscribers are websocket processes.
2. It saves subscriber's process ID to mnesia persistent DB

Once websocket process receives connection request, it saves its process ID to persistent DB
and once new message received, it forwards it to open websocket connection.


How to scale this system
========================

Mnesia is a distributed, fault-tolerant DBMS. Tables can be replicated on different Erlang nodes in various ways.
But in real world application, production-grade object storage should be used instead. For example Riak CS or CouchDB.
