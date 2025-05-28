{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api (Spell(..), API, FilterBy(..)) where

import Servant
import Data.Text (toLower)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Schema (Spell(..))

type API = "spell" :> QueryParam "filterby" FilterBy :> Get '[JSON] [Spell]

data FilterBy = NotApproved | OwnSpells
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance FromHttpApiData FilterBy where
  parseQueryParam txt = case toLower txt of
                          "notapproved" -> Right NotApproved
                          "ownspells" -> Right OwnSpells
                          _ -> Left "Invalid FilterBy value (NotApproved or OwnSpells) <- FromHttpApiData instance"

instance ToHttpApiData FilterBy where
  toQueryParam NotApproved = "notapproved" 
  toQueryParam OwnSpells = "ownspells" 

