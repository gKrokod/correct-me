{-# LANGUAGE RecordWildCards #-}

module Handlers.Database.Spell.Check (checkSpellBase) where

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Either (isLeft)
import Handlers.Database.Spell (Handle (..))
import Handlers.Logger (Log (..), logMessage)
import Data.Text as T (Text,pack)
import Handlers.Web.Spell.Types (CheckSpellInternal(..))

checkSpellBase :: (Monad m) => Handle m -> CheckSpellInternal -> m (Either Text ())
checkSpellBase h checkSp@(CheckSpellInternal {..}) = do
  let logHandle = logger h
  existPhrase <- findPhrase h phrase
  when (isLeft existPhrase) (logMessage logHandle Error "function findPhrase fail")
  existUser <- findUserByName h client
  when (isLeft existUser) (logMessage logHandle Error "function findUserByName fail")
  existSpell <- findSpellById h idSpell
  when (isLeft existSpell) (logMessage logHandle Error "function findSpellById fail")
  phraseFromSpell <- validCheck h checkSp
  when (isLeft phraseFromSpell) (logMessage logHandle Error "function phraseFromSpell")
  case (existPhrase, existUser, existSpell, phraseFromSpell) of
    (Right (Just _), Right (Just _), Right (Just _), Right True) -> do
          tryCheck <- checkSpell h checkSp
          when (isLeft tryCheck) (logMessage logHandle Warning "function checkSpell fail")
          pure $ either (Left . T.pack . displayException) Right tryCheck
    _ -> do
      logMessage logHandle Warning ("Fail to check spell")
      pure $ Left "fail to check spell"
