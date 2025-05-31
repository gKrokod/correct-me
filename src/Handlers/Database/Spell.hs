module Handlers.Database.Spell (Handle (..)) where

import Control.Exception (SomeException)
import qualified Handlers.Logger
import Handlers.Web.Spell.Types
import Web.DTO.Spell 
import Schema 
import Data.Text (Text)
import Web.Types
import Data.Int (Int64)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    pullSpells :: Client -> Maybe FilterBy -> m (Either SomeException [SpellToWeb]),
    putSpell :: SpellInternal -> m (Either SomeException ()),
    addPhrase :: PhraseInternal -> m (Either SomeException ()),
    checkSpell :: CheckSpellInternal -> m (Either SomeException ()),
    validCheck :: CheckSpellInternal -> m (Either SomeException Bool),
    findUserByName :: Client -> m (Either SomeException (Maybe User)),
    findSpellById :: Int64 -> m (Either SomeException (Maybe Spell)),
    findPhrase :: Text -> m (Either SomeException (Maybe Phrase)),
    createUser :: Client -> m (Either SomeException ())
  }
