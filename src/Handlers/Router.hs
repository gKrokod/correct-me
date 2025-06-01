module Handlers.Router (doLogic, doAuthorization) where

import qualified Data.ByteString as B
import qualified Handlers.Logger
import Handlers.Web.Api (endPointSpell)

import Handlers.Web.Base (Handle (..))
import Network.Wai (Request, Response, rawPathInfo, requestHeaders)
import Web.Query (headersToLoginAndPassword)
import Web.Types (Client (..))
import qualified Web.Utils as WU

doAuthorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
doAuthorization h req = do
  let secureData = headersToLoginAndPassword . requestHeaders $ req
  case secureData of
    Nothing -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Error "Request don't have Login"
      pure . Left $ WU.response403
    Just (clientName, _) -> do
      let h' = h {client = Just $ Client clientName}
      pure $ Right h'

doLogic :: (Monad m) => Handle m -> Request -> m Response
doLogic h req = do
  case rawPathInfo req of
    path
      | B.isPrefixOf "/spell" path -> endPointSpell h req
      | otherwise -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
          pure WU.response404
