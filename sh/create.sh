#!/bin/bash
curl -v -X POST user1@127.0.0.1:4221/spell/create -H "Content-Type: application/json" -d '{"phrase":"New Phrase for a nice fox"}'


