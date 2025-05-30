module Schema (EntityField(..),  User(..),Phrase(..),Spelling(..), Spell(..), Unique(..))  where

import Database.Migrations.Migrationv0 (EntityField(..), User(..),Phrase(..),Spelling(..), Spell(..), Unique(..))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (toLower)


-- instance FromHttpApiData FilterBy where
--   parseQueryParam txt = case toLower txt of
--                           "notapproved" -> Right NotApproved
--                           "ownspells" -> Right OwnSpells
--                           _ -> Left "Invalid FilterBy value (NotApproved or OwnSpells) <- FromHttpApiData instance"
--
-- instance ToHttpApiData FilterBy where
--   toQueryParam NotApproved = "notapproved" 
--   toQueryParam OwnSpells = "ownspells" 
--
