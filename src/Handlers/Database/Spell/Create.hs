{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.Spell.Create (createSpellBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Text as T (Text,pack)
import Handlers.Database.Spell (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell.Types (SpellInternal (..))
-- import Types (Label (..), Login (..), Title (..))

import Handlers.Web.Spell.Types (SpellInternal (..))
import Network.Wai (Request, Response)


createSpellBase :: (Monad m) => Handle m -> SpellInternal -> m (Either Text ())
createSpellBase h spell@(SpellInternal {..}) = do
  let logHandle = logger h
  (logMessage logHandle Debug "CreareSpellBase")

  existPhrase <- findPhrase h phrase
  when (isLeft existPhrase) (logMessage logHandle Error "function findPhrase fail")

  existUser <- findUserByName h author
  when (isLeft existUser) (logMessage logHandle Debug "function findUserByName fail")

  case (existPhrase, existUser) of
    (Right Nothing, Right Nothing) -> do
      newUser <- createUser h author
      case newUser of
        Left _ -> undefined
        Right _ -> do
          tryPut <- putSpell h spell
          when (isLeft tryPut) (logMessage logHandle Error "function putSpell fail")
          pure $ either (Left . T.pack . displayException) Right tryPut
    (Right Nothing, Right (Just _user)) -> do
          tryPut <- putSpell h spell
          when (isLeft tryPut) (logMessage logHandle Error "function putSpell fail")
          pure $ either (Left . T.pack . displayException) Right tryPut
    _ -> do
      logMessage logHandle Warning ("Fail to create spell with phrase")
      pure $ Left "fail to create spell"


--           baseHandle
--           ( SpellInternal
--               { phrase = phrase,
--                 author = author,
--                 revision = revi
--               }
--           )
--       case tryCreateSpell of
--         Right _ ->
--           pure WU.response200
--         Left e -> do
--           Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
--           pure WU.response500
