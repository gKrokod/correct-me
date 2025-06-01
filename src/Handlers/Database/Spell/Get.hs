module Handlers.Database.Spell.Get (getAllSpells) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Text as T (Text, pack)
import Handlers.Database.Spell (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Web.DTO.Spell (SpellToWeb (..))
import Web.Types (Client, FilterBy)

getAllSpells :: (Monad m) => Handle m -> Client -> Maybe FilterBy -> m (Either Text [SpellToWeb])
getAllSpells h author filterBy = do
  let logHandle = logger h
  spells <- pullSpells h author filterBy
  when (isLeft spells) (logMessage logHandle Handlers.Logger.Error "function pullSpells fail")
  pure $ either (Left . T.pack . displayException) Right spells
