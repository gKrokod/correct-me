module Handlers.Database.Spell (Handle (..)) where

import Control.Exception (SomeException)
import Database.Persist.Sql (PersistentSqlException)
import qualified Handlers.Logger
import Handlers.Web.Spell.Types
import Web.DTO.Spell 
import Schema 
import Data.Text (Text)
import Web.Query
import Web.Types


data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    pullSpells :: Client -> Maybe FilterBy -> m (Either SomeException [SpellToWeb])   --user name -> filter ->...
      --addSpell
      --checkSpell
      --createSpell
  }
