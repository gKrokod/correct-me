{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Database.Migrations.Migrationv0 (EntityField (..), User (..), Phrase (..), Spelling (..), Spell (..), migrateVer0, Unique (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Database.Migrations.Type (MyMigration (..), createMigrateTable)
import Database.Persist.Class (EntityField (..), Unique (..))
import Database.Persist.TH (migrateModels)
import qualified Database.Persist.TH as PTH
import GHC.Generics (Generic)
import Web.Types (SpellResult)

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkEntityDefList "createTablesForEntity"]
  [PTH.persistLowerCase|
 User sql=users
  name Text
  UniqueUserName name
  deriving Eq Show Generic FromJSON ToJSON
 Phrase sql=phrases
  text Text
  userId UserId
  spellingId SpellingId
  UniquePhraseText text
  deriving Eq Show Generic FromJSON ToJSON
 Spelling sql=spelling
  revisions SpellResult 
  deriving Show Generic FromJSON ToJSON
 Spell sql=spells
  phraseId PhraseId
  paraphrasesId [PhraseId]
  isApproved Bool
  deriving Eq Show Generic FromJSON ToJSON
|]

migrateVer0 :: MyMigration
migrateVer0 = MkMigration {version = 0, description = "create all tables", content = migrateModels (createMigrateTable <> createTablesForEntity)}
