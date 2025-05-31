{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Spell.Check (checkSpell) where

import qualified Data.Text as T
import Handlers.Database.Api (checkSpellBase)
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell (Handle (..))
import Handlers.Web.Spell.Types (CheckSpellInternal(..))
import Network.Wai (Request, Response)
import Web.DTO.Spell  (CheckSpellFromWeb(..), webToCheckSpellFromWeb)
import qualified Web.Utils as WU
import Web.Types(Client)

checkSpell :: (Monad m) => Client -> Handle m -> Request -> m Response
checkSpell user h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToCheckSpellFromWeb <$> getBody h req
  case body of
    Left e -> do
      logMessage logHandle Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (CheckSpellFromWeb id _) | id < 1 -> do
      let e = "id from request < 1"
      logMessage logHandle Error e
      pure (WU.response400 e)
    Right (CheckSpellFromWeb {..}) -> do
      tryCheckSpell <-
        checkSpellBase
          baseHandle
          ( CheckSpellInternal { idSpell = id, client = user, phrase = phrase } )
      case tryCheckSpell of
        Right _ ->
          pure WU.response200
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure WU.response500
