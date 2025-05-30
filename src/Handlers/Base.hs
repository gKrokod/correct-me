module Handlers.Base where

import Control.Exception (SomeException)
import Data.Text (Text)
import qualified Handlers.Logger
import Schema
import Web.DTO.Spell
import Web.Query (FilterBy(..))


data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    pullSpells :: Text -> Maybe FilterBy -> m (Either SomeException [SpellToWeb])   --user name -> filter ->...
  }
