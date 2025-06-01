module Handlers.Web.Base (Handle (..)) where

import Database.Persist.Postgresql (ConnectionString)
import qualified Handlers.Logger
import qualified Handlers.Web.Spell (Handle (..))
import Web.Types (Client)

data Handle m = Handle
  { connectionString :: ConnectionString,
    logger :: Handlers.Logger.Handle m,
    spell :: Handlers.Web.Spell.Handle m, -- base this,
    client :: Maybe Client
  }
