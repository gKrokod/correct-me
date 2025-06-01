{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.Spell.Add (addPhraseBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Text as T (Text, pack)
import Handlers.Database.Spell (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell.Types (PhraseInternal (..), SpellInternal (..))

addPhraseBase :: (Monad m) => Handle m -> PhraseInternal -> m (Either Text ())
addPhraseBase h anotherPhrase@(PhraseInternal id SpellInternal {..}) = do
  let logHandle = logger h
  existPhrase <- findPhrase h phrase
  when (isLeft existPhrase) (logMessage logHandle Error "function findPhrase fail")
  existUser <- findUserByName h author
  when (isLeft existUser) (logMessage logHandle Error "function findUserByName fail")
  existSpell <- findSpellById h id
  when (isLeft existSpell) (logMessage logHandle Error "function findSpellById fail")
  case (existPhrase, existUser, existSpell) of
    (Right Nothing, Right Nothing, Right (Just _sp)) -> do
      newUser <- createUser h author
      case newUser of
        Left _ -> do
          logMessage logHandle Error "Can't create new user"
          pure $ Left "fail to add phrase"
        Right _ -> do
          tryPut <- addPhrase h anotherPhrase
          when (isLeft tryPut) (logMessage logHandle Error "function addPhrase fail")
          pure $ either (Left . T.pack . displayException) Right tryPut
    (Right Nothing, Right (Just _user), Right (Just _sp)) -> do
      tryPut <- addPhrase h anotherPhrase
      when (isLeft tryPut) (logMessage logHandle Error "function addPhrase fail")
      pure $ either (Left . T.pack . displayException) Right tryPut
    _ -> do
      logMessage logHandle Warning "Fail to add phrase"
      pure $ Left "fail to add phrase"
