#!/bin/bash

#
# Queries user's token
#
# Uses that token to upload file
#
# Retrieves uploaded file by its version id
#

TOKEN=$(curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"username":"example@example.com","password":"blah"}' \
http://127.0.0.1:8001/auth-token/ | python3 -c 'import sys, json; token=json.load(sys.stdin);print(token["token"])')


THE_ID=$(curl -X POST http://127.0.0.1:8001/file_versions/ \
  -H "Authorization: Token "$TOKEN \
  -F version_number=1 \
  -F file=@Pipfile | python3 -c 'import sys, json; resp=json.load(sys.stdin);print(resp)')

echo $THE_ID

curl -X GET "http://127.0.0.1:8001/file_versions/"$THE_ID"/download/" \
  -H "Authorization: Token "$TOKEN
