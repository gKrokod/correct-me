#!/bin/bash
#
echo "\nBare request"
curl 'user3@127.0.0.1:4221/spell/get'
echo "\nFilter by NotApproved"
curl 'user3@127.0.0.1:4221/spell/get?filter=%22NotApproved%22'
echo "\nFilter by Own"
curl 'user4@127.0.0.1:4221/spell/get?filter=%22OwnSpells%22'

