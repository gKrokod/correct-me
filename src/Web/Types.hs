{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Types (Client (..), FilterBy (..), FilterFromWeb (..), SpellRevision (..), SpellResult, Id(..), TextPhrase(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)
import qualified Database.Persist.TH as PTH

newtype Client = Client {name :: Text}
newtype Id = MkId {giveId :: Int64}
newtype TextPhrase = MkTextPhrase {giveText :: Text}

data FilterBy = NotApproved | OwnSpells -- OwnSpells and NotApproved?
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype FilterFromWeb = FilterFromWeb (Maybe FilterBy)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpellRevision = MkSpellRevision
  { code :: Int, -- code — код ошибки
    pos :: Int, -- pos — позиция слова с ошибкой (отсчет от 0)
    row :: Int, -- row — номер строки (отсчет от 0)
    col :: Int, -- col — номер столбца (отсчет оn 0)
    len :: Int, -- len — длина слова с ошибкой.
    word :: Text, -- word — исходное слово
    s :: [Text] -- s — подсказка (может быть несколько или могут отсутствовать).
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type SpellResult = [SpellRevision]

PTH.derivePersistFieldJSON "SpellResult"
