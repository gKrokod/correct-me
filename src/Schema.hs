module Schema (EntityField(..), SpellRevision(..), User(..),Phrase(..),Spelling(..), Spell(..), SpellResult, FilterBy(..))  where

import Database.Migrations.Migrationv0 (EntityField(..), SpellRevision(..), User(..),Phrase(..),Spelling(..), Spell(..), SpellResult)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Servant
import Data.Text (toLower)

data FilterBy = NotApproved | OwnSpells -- OwnSpells and NotApproved?
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

