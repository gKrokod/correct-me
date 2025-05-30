{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.Spell.Create (createSpellBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Text as T (Text,pack)
import Handlers.Database.Spell (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell.Types (SpellInternal (..))
import Handlers.Web.Spell.Types (SpellInternal (..))
import Network.Wai (Request, Response)


createSpellBase :: (Monad m) => Handle m -> SpellInternal -> m (Either Text ())
createSpellBase h spell@(SpellInternal {..}) = do
  let logHandle = logger h
  existPhrase <- findPhrase h phrase
  when (isLeft existPhrase) (logMessage logHandle Error "function findPhrase fail")
  existUser <- findUserByName h author
  when (isLeft existUser) (logMessage logHandle Error "function findUserByName fail")
  case (existPhrase, existUser) of
    (Right Nothing, Right Nothing) -> do
      newUser <- createUser h author
      case newUser of
        Left _ -> do
          logMessage logHandle Error ("Can't create new user")
          pure $ Left "fail to create spell"
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
