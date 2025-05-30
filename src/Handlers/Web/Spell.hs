module Handlers.Web.Spell (Handle (..)) where

import qualified Data.ByteString as B
-- import qualified Handlers.Database.Auth
import qualified Handlers.Database.Spell
import qualified Handlers.Logger
import Network.Wai (Request)
import Data.Text (Text)
import Schema 
import Web.Types (SpellResult)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Spell.Handle m,
    getBody :: Request -> m B.ByteString,
    revisionSpell :: Text -> m SpellResult
  }



-- data Handle m = Handle
--   { connectionString :: ConnectionString,
--     logger :: Handlers.Logger.Handle m,
--     spell :: Handlers.Web.Spell.Handle m, -- base this,
--     revisionSpell :: Text -> m Bool
--     -- client :: Handlers.Service.Yandex.Client m -- tyt clienta potom
--   }
