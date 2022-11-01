#!/bin/bash
#
# This is supposed to be an integration test, checking inter-service connectivity and game logic
#
ADMIN_SECRET=IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct
AUTH_USER_API=31.28.168.162:5024
BATTLE_API=31.28.168.162:5100

# create the first user

JSON=$(curl -s -X POST "http://$AUTH_USER_API/v1/auth/users" \
    -H "accept: application/json" \
    -H "authorization: IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct" \
    -H "Content-Type: application/json" \
    -d "{ \"email\": \"vb@xentime.com\", \"name\": \"Vitalii\", \"role\": \"User\", \"password\": \"string\"}")

EMAIL=`echo $JSON|python -c "import sys, json; print json.load(sys.stdin)['data']['email']"`
[ "$EMAIL" = "vb@xentime.com" ] || {
    echo "Expected vb@xentime.com, received: $EMAIL"
    exit -1
}

# create a first player
JSON=$(curl -s -X POST "http://$AUTH_USER_API/v1/auth/players" \
    -H "accept: application/json" \
    -H "authorization: IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct" \
    -H "Content-Type: application/json" \
    -d "{ \"name\": \"Vitalii\", \"description\": \"Obviously serious player\", \"gold\": 100, \"silver\": 100, \"attack_value\": 100, \"hit_points\": 100, \"luck_value\": 30}")

NAME=`echo $JSON|python -c "import sys, json; print json.load(sys.stdin)['data']['name']"`
[ "$NAME" = "Vitalii" ] || {
    echo "Expected Vitalii, received: $NAME"
    exit -1
}

# create a second player
JSON=$(curl -s -X POST "http://$AUTH_USER_API/v1/auth/players" \
    -H "accept: application/json" \
    -H "authorization: IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct" \
    -H "Content-Type: application/json" \
    -d "{ \"name\": \"John\", \"description\": \"Serious player\", \"gold\": 100, \"silver\": 100, \"attack_value\": 100, \"hit_points\": 100, \"luck_value\": 30}")

NAME=`echo $JSON|python -c "import sys, json; print json.load(sys.stdin)['data']['name']"`
[ "$NAME" = "John" ] || {
    echo "Expected Vitalii, received: $NAME"
    exit -1
}

ALL_PLAYERS = $(curl -X GET "http://31.28.168.162:5024/v1/auth/players" \
    -H "accept: application/json" \
    -H "authorization: IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct")
PLAYER_ONE_ID=`echo $JSON|python -c "import sys, json; print [i['id'] for i in json.load(sys.stdin)['data']['all_players'] if i['name'] == 'Vitalii'][0]"`
PLAYER_TWO_ID=`echo $JSON|python -c "import sys, json; print [i['id'] for i in json.load(sys.stdin)['data']['all_players'] if i['name'] == 'John'][0]"`

# Create battle
JSON=$(curl -s -X POST "http://$BATTLE_API/v1/battle/battles" \
    -H "accept: application/json" \
    -H "authorization: IphJogitvedOaSwyujAwunbibcoovLillyawubTyiminnidEebbiecAgDapdacBenCanmydLumJecOct" \
    -H "Content-Type: application/json" \
    -d "{ \"player1_id\": \"${PLAYER_ONE_ID}\", \"player2_id\": \"${PLAYER_TWO_ID}\", \"player1_health\": 100, \"player2_health\": 100, \"is_finished\": false}")

BATTLE_ID=`echo $JSON|python -c "import sys, json; print json.load(sys.stdin)['data']['battle_id']"`
echo "Battle has been created successfully ! $BATTLE_ID"
