module Database.Api (migrationEngine, pullSpells, putSpell, createUser, findUserByName, findPhrase)  where

import Database.Migrations.Migration (migrationEngine)
-- import Database.Queries.Spell 
import Database.Queries.Get (pullSpells)
import Database.Queries.Create (putSpell, createUser)
import Database.Queries.Find (findUserByName, findPhrase)
--
