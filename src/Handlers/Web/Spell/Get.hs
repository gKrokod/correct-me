module Handlers.Web.Spell.Get (existingSpells) where

import Handlers.Database.Api (getAllSpells)
import Handlers.Logger (Log (..), logMessage)
import Handlers.Web.Spell (Handle (..))
import Network.Wai (Request, Response, queryString)
import Web.DTO.Spell (spellsToWeb)
import Web.Query (queryToFilter)
import Web.Types (Client)
import qualified Web.Utils as WU

existingSpells :: (Monad m) => Client -> Handle m -> Request -> m Response
existingSpells author h req = do
  let logHandle = logger h
      baseHandle = base h
      query = queryString req
      filterBy = queryToFilter query
  spells <- getAllSpells baseHandle author filterBy
  case spells of
    Left e -> do
      logMessage logHandle Error e
      pure WU.response500
    Right spells' -> do
      pure . WU.mkGoodResponse . spellsToWeb $ spells'
