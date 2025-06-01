{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Spell.Create (createSpell) where

import Control.Monad (when)
import Data.Either (isLeft, fromLeft, fromRight)
import qualified Data.Text as T
import Handlers.Database.Api (createSpellBase)
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell (Handle (..))
import Handlers.Web.Spell.Types (SpellInternal (..))
import Network.Wai (Request, Response)
import Web.DTO.Spell (PhraseFromWeb (..), webToPhrase)
import Web.Types (Client, TextPhrase(..))
import qualified Web.Utils as WU

createSpell :: (Monad m) => Client -> Handle m -> Request -> m Response
createSpell author h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToPhrase <$> getBody h req
  case body of
    Left e -> do
      logMessage logHandle Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (PhraseFromWeb phrase) -> do
      revi <- revisionSpell h phrase
      when (isLeft revi) (logMessage logHandle Warning (fromLeft "function revisionSpell fail" revi))
      tryCreateSpell <-
        createSpellBase
          baseHandle
          ( SpellInternal
              { phrase = MkTextPhrase phrase,
                author = author,
                revision = fromRight [] revi
              }
          )
      case tryCreateSpell of
        Right _ ->
          pure WU.response200
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure WU.response500
