{-# LANGUAGE DataKinds #-}

module Handlers.Web.Base  where
import Data.Text (Text)

import Database.Persist.Postgresql (ConnectionString)
import qualified Handlers.Logger
import qualified Handlers.Web.Spell (Handle (..))
import Web.Types (Client)


data Handle m = Handle
  { connectionString :: ConnectionString,
    logger :: Handlers.Logger.Handle m,
    spell :: Handlers.Web.Spell.Handle m, -- base this,
    client :: Maybe Client,
    revisionSpell :: Text -> m Bool
    -- client :: Handlers.Service.Yandex.Client m -- tyt clienta potom
  }
