#!/bin/bash
echo "\nCheck 3 spell. User from base"
curl -X POST user4@127.0.0.1:4221/spell/check -H "Content-Type: application/json" -d '{"id":3,"phrase":"Angless"}'
echo "\nCheck. Unknow User"
curl -X POST u1@127.0.0.1:4221/spell/check -H "Content-Type: application/json" -d '{"id":3,"phrase":"angle"}'
echo "\nTry Check 1 spell. User from base"
curl -X POST user4@127.0.0.1:4221/spell/check -H "Content-Type: application/json" -d '{"id":1,"phrase":"angle"}'
echo "\nadd New phrase for 1 spell"
curl -X POST user1@127.0.0.1:4221/spell/add -H "Content-Type: application/json" -d '{"id":1,"phrase":"New Phrash1"}'
echo "\nCheck 1 spell again."
curl -X POST user1@127.0.0.1:4221/spell/check -H "Content-Type: application/json" -d '{"id":1,"phrase":"New Phrash1"}'
