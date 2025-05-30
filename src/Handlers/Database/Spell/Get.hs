module Handlers.Database.Spell.Get (getAllSpells) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
-- import Handlers.Database.Base (Limit (..), Offset (..))
import Handlers.Database.Spell (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Schema
import Data.Text as T (Text, pack)
import Web.DTO.Spell (PhraseToWeb(..), SpellToWeb(..))

getAllSpells :: (Monad m) => Handle m -> Text -> Text -> m (Either Text [SpellToWeb])
getAllSpells h a f = do
  let logHandle = logger h
  spells <- pullSpells h a Nothing
  when (isLeft spells) (logMessage logHandle Handlers.Logger.Error "function pullSpells fail")
  pure $ either (Left . T.pack . displayException) Right spells
  --
-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     pullSpells :: Text -> Maybe FilterBy -> m (Either SomeException [SpellToWeb])   --user name -> filter ->...
--       --addSpell
--       --checkSpell
--       --createSpell
--   }
