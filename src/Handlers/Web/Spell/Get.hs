module Handlers.Web.Spell.Get (existingSpells) where

import Handlers.Database.Api (getAllSpells)
-- import Handlers.Database.Base (Limit (..), Offset (..))
import qualified Handlers.Logger
import Handlers.Web.Spell (Handle (..))
import Network.Wai (Request, Response, queryString)
import Schema 
import Data.Text as T (Text,pack)
import Web.DTO.Spell (spellsToWeb)
-- import Web.Query (queryToFilters, queryToFind, queryToPaginate, queryToSort)
import qualified Web.Utils as WU
import Web.Query


existingSpells :: (Monad m) => Text -> Handle m -> Request -> m Response
existingSpells author h req = do
  let logHandle = logger h
      baseHandle = base h
      query = queryString req
      filterBy = queryToFilter query
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "existingSPells - here where are"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show filterBy)
  spells <- getAllSpells baseHandle author filterBy
  -- news <- getAllSpells baseHandle (MkOffset userOffset) (MkLimit userLimit) userSortColumn userSortOrder findSubString filters
  case spells of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure WU.response500
    Right spells' -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "right spells is databse"
      pure . WU.mkGoodResponse . spellsToWeb $ spells'
