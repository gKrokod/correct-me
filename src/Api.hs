{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Data.Text (Text, toLower)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)


type API = "spell" :> QueryParam "filterby" FilterBy :> Get '[JSON] [Spell]

data Sp = Sp {
  spellText :: Text, --(approved?) -- spell = Main Text [Text]
  isApproved :: Bool,
  author :: Text, -- UserName
  var :: (Text, [(Text,[Text])]) -- (phrase [(Word of phrase, [Maybe Word)])
             }

data Spell = Spell Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
