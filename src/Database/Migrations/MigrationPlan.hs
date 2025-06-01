module Database.Migrations.MigrationPlan (migrationPlan) where

import Database.Migrations.Migrationv0 (migrateVer0)
import Database.Migrations.Migrationv1 (migrateVer1)
import Database.Migrations.Type (MyMigration (..))

type MigrationPlan = [MyMigration]

migrationPlan :: MigrationPlan
migrationPlan = [migrateVer0, migrateVer1]
