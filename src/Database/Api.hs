module Database.Api (migrationEngine, checkSpell, addPhrase, pullSpells, putSpell, createUser, findUserByName, findPhrase, findSpellById, validCheck)  where

import Database.Migrations.Migration (migrationEngine)
import Database.Queries.Get (pullSpells)
import Database.Queries.Create (putSpell, createUser)
import Database.Queries.Add (addPhrase)
import Database.Queries.Check (checkSpell, validCheck)
import Database.Queries.Find (findUserByName, findPhrase, findSpellById)
--
