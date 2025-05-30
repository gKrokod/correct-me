module Database.Api (migrationEngine, addPhrase, pullSpells, putSpell, createUser, findUserByName, findPhrase, findSpellById)  where

import Database.Migrations.Migration (migrationEngine)
-- import Database.Queries.Spell 
import Database.Queries.Get (pullSpells)
import Database.Queries.Create (putSpell, createUser)
import Database.Queries.Add (addPhrase)
import Database.Queries.Find (findUserByName, findPhrase, findSpellById)
--
