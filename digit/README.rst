Authentication Service
======================

This is the Digit's test battle application. It consists of two microservices.

1. ``test_auth`` -- exposes authentication and authorization API endpoints

2. ``test_battle`` -- the battle processor, that accepts requests to create a new battle
   and then requests for battle-related logic.

The reason two different microservices were created: in real application they would 
scale differently ( by docker swarm or kubernetes ).


How to use it
=============

You will need docker installed ( docker-ce package ).
sources.list entry example:
"""
deb [arch=amd64] https://download.docker.com/linux/debian stretch stable
"""

Then you can start it with the following command.
"""
docker-compose up
"""

How it works
============

Both microservices have Swagger UI enabled, for convenience of use.
You can send send HTTP requests using Swagger web ui.

Flask application server is supervised by gunicorn, inside docker container.
By default 10 gunicorn processes are spwaned.

Flask application servers query two different Redis databases, configured with persistency.
( Two databases are necessary for a better scaling ).

Ther's an additional docker container for an asynchronous battle processor, called battle_worker.
It has rq's worker script running inside, taking tasks from Redis queue and executing them.

In order to execute requests, you will need a Authentication Token.
Use ``ADMIN_SECRET``, defined in docker-env/docker_auth.txt for creating first users
or receiving the first token.

For example:
""""
curl -X POST "http://31.28.168.162:5024/v1/auth/tokens" \
    -H "accept: application/json" \
    -H "authorization: IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct" \
    -H "Content-Type: application/json" \
    -d "{ \"email\": \"vb@xentime.com\", \"password\": \"string\"}"
"""

``authorization`` header contains the ``ADMIN_SECRET`` token in this example.

Expected response example:
"""
{
  "data": {
    "id": "a575ac2e-5912-46f8-b1d8-3c16c869ead5",
    "issued_at": "2020-07-17T12:10:26.451317",
    "expires": "2020-07-18T12:10:26.451332",
    "user_id": null
  },
  "version": "1"
}
"""

Token "a575ac2e-5912-46f8-b1d8-3c16c869ead5" can now be used for authorization.

The battle microservice calls authentication microservice using ADMIN_SECRET token,
to retrieve information on players and to change them.


docker-compose.yml has logging configured in JSON format, so logs can be picked up by telegraf agent and stored
into influxdb, in order to analyze/aggregate them and to display them with Grafana later on.
