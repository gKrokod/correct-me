module Handlers.Web.Spell.Get (existingSpells) where

import Handlers.Database.Api (getAllSpells)
-- import Handlers.Database.Base (Limit (..), Offset (..))
import qualified Handlers.Logger
import Handlers.Web.Spell (Handle (..))
import Network.Wai (Request, Response, queryString)
import Schema 
import Data.Text (Text)
import Web.DTO.Spell (spellsToWeb)
-- import Web.Query (queryToFilters, queryToFind, queryToPaginate, queryToSort)
import qualified Web.Utils as WU

type FilterItem = Text

existingSpells :: (Monad m) => Text -> FilterItem -> Handle m -> Request -> m Response
existingSpells author filterBy h req = do
  let logHandle = logger h
      baseHandle = base h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "existingSPells - here where are"
  --     query = queryString req
  --     (userOffset, userLimit) = queryToPaginate query
  --     (userSortColumn, userSortOrder) = queryToSort query
  --     findSubString = queryToFind query
  --     filters = filterAuthor : queryToFilters query
  spells <- getAllSpells baseHandle author filterBy
  -- news <- getAllSpells baseHandle (MkOffset userOffset) (MkLimit userLimit) userSortColumn userSortOrder findSubString filters
  case spells of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure WU.response500
    Right spells' -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "right spells is databse"
      pure . WU.mkGoodResponse . spellsToWeb $ spells'
