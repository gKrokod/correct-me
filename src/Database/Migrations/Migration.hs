{-# LANGUAGE RecordWildCards #-}

module Database.Migrations.Migration (migrationEngine) where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (mapMaybe)
import Data.Time (getCurrentTime)
import Database.Esqueleto.Experimental (insert_)
import Database.Migrations.MigrationPlan (migrationPlan)
import Database.Migrations.Type (MigrateTable (..), MyMigration (..))
import Database.Persist.Postgresql (ConnectionString, rawExecute)
import Database.Persist.Sql (SqlPersistT, runMigration)
import Database.Queries.MigrateTable (isMigrateTable)
import Database.Verb (runDataBaseWithOutLog)

type Version = Int

migrationEngine :: ConnectionString -> IO ()
migrationEngine pginfo = do
  -- todo remove at last
  putStrLn "Drop All tables"
  runDataBaseWithOutLog pginfo dropAll

  lastMigration <- isMigrateTable pginfo
  let migrationPlan' = mapMaybe (predicate lastMigration) migrationPlan

  runDataBaseWithOutLog pginfo $ do
    mapM_ applyMigrate migrationPlan'
  where
    applyMigrate m@(MkMigration {..}) = do
      liftIO . putStrLn $ "Apply migration " <> show m
      runMigration content
      time <- liftIO getCurrentTime
      insert_ (MigrateTable {migrateTableVersion = version, migrateTableDescription = description, migrateTableTime = time})

    predicate :: Either SomeException (Maybe Version) -> MyMigration -> Maybe MyMigration
    predicate (Right (Just num)) (MkMigration {..}) | num >= version = Nothing
    predicate _ m = Just m

dropAll :: (MonadIO m) => SqlPersistT m ()
dropAll = rawExecute "DROP TABLE IF EXISTS users, phrases, spelling, spells, migrate_table" []
