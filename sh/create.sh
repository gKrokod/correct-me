#!/bin/bash
echo "\nCreate new spell. user from base"
curl -X POST user1@127.0.0.1:4221/spell/create -H "Content-Type: application/json" -d '{"phrase":"New Phrase1 for a nice fox. Bimbo Bambo. Iresh terrier. Irish terriar. brat. willow"}'
echo "\nCreate new spell. Unknow user"
curl -X POST user5@127.0.0.1:4221/spell/create -H "Content-Type: application/json" -d '{"phrase":"Reduco"}'

