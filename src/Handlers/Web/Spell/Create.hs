{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Web.Spell.Create (createSpell) where

import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (createSpellBase)
-- import Handlers.Database.Auth (ClientRole (..))
import qualified Handlers.Logger
import Handlers.Web.Spell (Handle (..))
-- import Handlers.Web.News.Types (NewsInternal (..))
import Network.Wai (Request, Response)
-- import Types (Content (..), Label (..), Login (..), Title (..))
-- import Web.DTO.News (NewsFromWeb (..), webToNews)
import qualified Web.Utils as WU

-- creatSpell :: (Monad m) => Proxy 'PublisherRole -> Handle m -> Request -> m Response
createSpell _ h req = do
  undefined
  -- let logHandle = logger h
  --     baseHandle = base h
  -- body <- webToNews <$> getBody h req
  -- case body of
  --   Left e -> do
  --     Handlers.Logger.logMessage logHandle Handlers.Logger.Error (T.pack e)
  --     pure (WU.response400 . T.pack $ e)
  --   Right (NewsFromWeb {..}) -> do
  --     tryCreateNews <-
  --       createNewsBase
  --         baseHandle
  --         ( NewsInternal
  --             { titleNews = MkTitle title,
  --               authorNews = MkLogin login,
  --               labelNews = MkLabel label,
  --               contentNews = MkContent content,
  --               imagesNews = images,
  --               isPublishNews = isPublish
  --             }
  --         )
  --     case tryCreateNews of
  --       Right _ ->
  --         pure WU.response200
  --       Left e -> do
  --         Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
  --         pure WU.response500
