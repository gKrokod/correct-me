{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Spell.Create (createSpell) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createSpellBase)
import qualified Handlers.Logger
import Handlers.Web.Spell (Handle (..))
import Handlers.Web.Spell.Types (SpellInternal (..))
import Network.Wai (Request, Response)
-- import Types (Content (..), Label (..), Login (..), Title (..))
import Web.DTO.Spell  (PhraseFromWeb(..), webToPhrase)
import qualified Web.Utils as WU
import Web.Types(Client)

createSpell :: (Monad m) => Client -> Handle m -> Request -> m Response
createSpell author h req = do
  let logHandle = logger h
      baseHandle = base h
  body <- webToPhrase <$> getBody h req
  case body of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
      pure (WU.response400 . T.pack $ e)
    Right (PhraseFromWeb phrase) -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Right "
      revi <- revisionSpell h phrase 
    -- revisionSpell :: Text -> m SpellResult
    -- revisionSpell :: Text -> m SpellResult
--    est li client
--    spelling nado get
      tryCreateSpell <-
        createSpellBase
          baseHandle
          ( SpellInternal
              { phrase = phrase,
                author = author,
                revision = revi
              }
          )
      case tryCreateSpell of
        Right _ ->
          pure WU.response200
        Left e -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
          pure WU.response500

-- import Network.Wai (Request, Response, rawPathInfo, requestHeaders)
-- import Web.Types(Client)
--
-- existingSpells :: (Monad m) => Client -> Handle m -> Request -> m Response
-- existingSpells author h req = do
--   let logHandle = logger h
--       baseHandle = base h
--       query = queryString req
--       filterBy = queryToFilter query
--   logMessage logHandle Debug "existingSPells - here where are"
--   logMessage logHandle Debug (T.pack $ show filterBy)
--   spells <- getAllSpells baseHandle author filterBy
--   case spells of
--     Left e -> do
--       logMessage logHandle Error e
--       pure WU.response500
--     Right spells' -> do
--       logMessage logHandle Debug "right spells is databse"
--       pure . WU.mkGoodResponse . spellsToWeb $ spells'
