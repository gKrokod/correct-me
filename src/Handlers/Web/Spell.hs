module Handlers.Web.Spell (Handle (..)) where

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Handlers.Database.Spell
import qualified Handlers.Logger
import Network.Wai (Request)
import Web.Types (SpellResult)

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Spell.Handle m,
    getBody :: Request -> m B.ByteString,
    revisionSpell :: Text -> m (Either Text SpellResult)
  }
