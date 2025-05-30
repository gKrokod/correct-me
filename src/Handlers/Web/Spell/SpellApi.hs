module Handlers.Web.Spell.SpellApi (endPointSpell) where

-- import qualified Handlers.Database.Auth as A (Client (..))
import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Handlers.Web.Spell.Create
import Handlers.Web.Spell.Get (existingSpells)
import Handlers.Web.Spell.Add 
import Handlers.Web.Spell.Check 
import Network.Wai (Request, Response, rawPathInfo)
import Schema 
import qualified Web.Utils as WU

endPointSpell :: (Monad m) => Handle m -> Request -> m Response
endPointSpell h req = do
  let logHandle = Handlers.Web.Base.logger h
      spellHandle = Handlers.Web.Base.spell h
  --     userRole = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/spell/get" -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "/spell/get tyt mu"
      let author = undefined  --FilterPublishOrAuthor (fmap getLogin . A.author $ userRole)
      let filterBy = undefined  --FilterPublishOrAuthor (fmap getLogin . A.author $ userRole)
      existingSpells "" "filterBy" spellHandle req
    "/spell/create" -> do
      undefined
    "/spell/check" -> do
      undefined
    "/spell/add" -> do
      undefined
      -- case userRole of
      --   A.Client {A.clientPublisherToken = (Just publisherRole)} -> createNews publisherRole newsHandle req
      --   _ -> do
      --     Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
      --     pure WU.response403
    -- "/news/edit" ->
    --   case userRole of
    --     A.Client {A.author = (Just author_)} -> do
    --       updateNews author_ newsHandle req
    --     _ -> do
    --       Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
    --       pure WU.response403
    -- "/news" -> do
    --   let filterAuthor = FilterPublishOrAuthor (fmap getLogin . A.author $ userRole)
    --   existingNews filterAuthor newsHandle req
    _ -> do
      -- error "not end point"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure WU.response404

-- doLogic :: (Monad m) => Handle m -> Request -> m Response
-- doLogic h req = do
--   case rawPathInfo req of
--     path
--       | B.isPrefixOf "/get" path -> endPointGet h req
--       | B.isPrefixOf "/create" path -> endPointCreate h req
--       | B.isPrefixOf "/check" path -> endPointCheck h req
--       | B.isPrefixOf "/add" path -> endPointAdd h req
--       | otherwise -> do
--           Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
--           pure WU.response404
