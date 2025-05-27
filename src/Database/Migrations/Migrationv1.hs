module Database.Migrations.Migrationv1 (migrateVer1) where

import Database.Data.FillTables 
import Database.Esqueleto.Experimental (insertMany_)
import Database.Migrations.Type (MyMigration (..))
import Database.Persist.Sql.Migration (runSqlCommand)

migrateVer1 :: MyMigration
migrateVer1 = MkMigration {version = 1, description = "add test data", content = action}
  where
    action = runSqlCommand $ do
      insertMany_ [user1, user2,user3,user4]
      insertMany_ [spe1,spe2]
      insertMany_ [phrase1, phrase2]
      insertMany_ [spell1,spell2]

-- user1, user2, user3, user4 :: S.User
-- user1 = S.User "user1"
-- user2 = S.User "user2"
-- user3 = S.User "user3"
-- user4 = S.User "user4"
--
-- spe1, spe2 :: S.Spelling 
-- spe1 = S.Spelling "word1 []"
-- spe2 = S.Spelling "word2 [some,vam]"
--
-- phrase1,phrase2 :: S.Phrase
-- phrase1 = S.Phrase "first phrase" (toSqlKey 1) (toSqlKey 1)
-- phrase2 = S.Phrase "second phrase" (toSqlKey 2) (toSqlKey 2)
-- phrase3 = S.Phrase "third phrase" (toSqlKey 3) (toSqlKey 1)
--
--
-- spell1,spell2 :: S.Spell
-- spell1 = S.Spell (toSqlKey 1) False
-- spell2 = S.Spell (toSqlKey 2) True
