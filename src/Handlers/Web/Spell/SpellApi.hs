module Handlers.Web.Spell.SpellApi (endPointSpell) where

import qualified Handlers.Logger
import Handlers.Web.Base (Handle (..))
import Handlers.Web.Spell.Create (createSpell)
import Handlers.Web.Spell.Get (existingSpells)
import Handlers.Web.Spell.Add 
import Handlers.Web.Spell.Check 
import Network.Wai (Request, Response, rawPathInfo)
import qualified Web.Utils as WU

endPointSpell :: (Monad m) => Handle m -> Request -> m Response
endPointSpell h req = do
  let logHandle = Handlers.Web.Base.logger h
      spellHandle = Handlers.Web.Base.spell h
      clientName = Handlers.Web.Base.client h
  case rawPathInfo req of
    "/spell/get" -> do
      case clientName of
        Just name -> existingSpells name spellHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure WU.response403
    "/spell/create" -> do
      case clientName of
        Just name -> createSpell name spellHandle req
        _ -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "Access denied"
          pure WU.response403
    "/spell/check" -> do
      undefined
    "/spell/add" -> do
      undefined
    _ -> do
      -- error "not end point"
      Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
      pure WU.response404
