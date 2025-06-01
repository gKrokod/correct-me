module Database.Api (migrationEngine, checkSpell, addPhrase, pullSpells, putSpell, createUser, findUserByName, findPhrase, findSpellById, validCheck) where

import Database.Migrations.Migration (migrationEngine)
import Database.Queries.Add (addPhrase)
import Database.Queries.Check (checkSpell, validCheck)
import Database.Queries.Create (createUser, putSpell)
import Database.Queries.Find (findPhrase, findSpellById, findUserByName)
import Database.Queries.Get (pullSpells)

--
