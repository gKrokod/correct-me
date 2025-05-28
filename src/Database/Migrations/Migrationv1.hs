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
      insertMany_ [spelling1,spelling2,spelling3,spelling4,spelling5,spelling6]
      insertMany_ [phrase1, phrase2, phrase3, phrase4, phrase5, phrase6]
      insertMany_ [spell1,spell2,spell3]

