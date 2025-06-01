#!/bin/bash
echo "\nAdd phrase for 1 spell. User from base"
#!/bin/bash
#
curl -X POST user1@127.0.0.1:4221/spell/add -H "Content-Type: application/json" -d '{"id":1,"phrase":"New Phrase1 for first Spell"}'
echo "\nAdd phrase for 3 spell. Unknow user"
curl -X POST user6@127.0.0.1:4221/spell/add -H "Content-Type: application/json" -d '{"id":3,"phrase":"New Phrase3 for third Spell"}'
echo "\nAdd phrase for 1 spell. id < 1. User from base"
curl -X POST user1@127.0.0.1:4221/spell/add -H "Content-Type: application/json" -d '{"id":-2,"phrase":"New Phrase1 for -2 Spell"}'
