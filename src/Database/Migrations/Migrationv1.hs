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

