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
    pullSpells :: Client -> Maybe FilterBy -> m (Either SomeException [SpellToWeb]),
    putSpell :: SpellInternal -> m (Either SomeException ()),
    findUserByName :: Client -> m (Either SomeException (Maybe ())),
    findPhrase :: Text -> m (Either SomeException (Maybe ())),
    createUser :: Client -> m (Either SomeException ())
      --addSpell
      --checkSpell
      --createSpell
  }
-- findPhrase = undefined
-- createUser = undefined
-- findUserByName = undefined
-- putUser = undefined
-- putSpell = undefined
-- createSpellBase :: (Monad m) => Handle m -> SpellInternal -> m (Either Text ())
-- createSpellBase h spell@(SpellInternal {..}) = do
--   let logHandle = logger h
--   (logMessage logHandle Debug "CreareSpellBase")
--
--   existPhrase <- findPhrase h phrase
--   when (isLeft existPhrase) (logMessage logHandle Error "function findPhrase fail")
--
--   existUser <- findUserByName h author
--   when (isLeft existUser) (logMessage logHandle Debug "function findUserByName fail")
--
--   case (existPhrase, existUser) of
--     (Right Nothing, Right Nothing) -> do
--       newUser <- createUser h author
--       case newUser of
--         Left _ -> undefined
--         Right _ -> do
--           tryPut <- putSpell h spell
--           when (isLeft tryPut) (logMessage logHandle Error "function putSpell fail")
--           pure $ either (Left . T.pack . displayException) (\_ -> Right ()) tryPut
--     (Right Nothing, Right (Just _user)) -> do
--           tryPut <- putSpell h spell
--           when (isLeft tryPut) (logMessage logHandle Error "function putSpell fail")
--           pure $ either (Left . T.pack . displayException) Right tryPut
--     _ -> do
--       logMessage logHandle Warning ("Fail to create spell with phrase")
--       pure $ Left "fail to create spell"
