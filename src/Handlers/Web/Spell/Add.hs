{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Spell.Add (addPhrase) where

import Control.Monad
import Data.Either
import qualified Data.Text as T
import Handlers.Database.Api (addPhraseBase)
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell (Handle (..))
import Handlers.Web.Spell.Types (PhraseInternal (..), SpellInternal (..))
import Network.Wai (Request, Response)
import Web.DTO.Spell (AnotherPhraseFromWeb (..), webToAnotherPhrase)
import Web.Types (Client, Id(..), TextPhrase(..))
import qualified Web.Utils as WU

addPhrase :: (Monad m) => Client -> Handle m -> Request -> m Response
addPhrase author h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToAnotherPhrase <$> getBody h req
  case body of
    Left e -> do
      logMessage logHandle Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (AnotherPhraseFromWeb id _) | id < 1 -> do
      let e = "id from request < 1"
      logMessage logHandle Error e
      pure (WU.response400 e)
    Right (AnotherPhraseFromWeb {..}) -> do
      revi <- revisionSpell h phrase
      when (isLeft revi) (logMessage logHandle Warning (fromLeft "function revisionSpell fail" revi))
      tryAddPhrase <-
        addPhraseBase
          baseHandle
          (PhraseInternal {idSpell = MkId id, anotherPhrase = SpellInternal {phrase = MkTextPhrase phrase, author = author, revision = fromRight [] revi}})
      case tryAddPhrase of
        Right _ ->
          pure WU.response200
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure WU.response500
