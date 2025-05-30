module Database.Api (migrationEngine, pullSpells, putSpell)  where

import Database.Migrations.Migration (migrationEngine)
-- import Database.Queries.Spell 
import Database.Queries.Get (pullSpells)
import Database.Queries.Create (putSpell)
--
