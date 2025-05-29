module Server () where

import Api
import Database.Data.FillTables  (spell1,spell2,spell3)
import Servant
import qualified Database.Api as DA
import DTO
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Handlers.Base (Handle(..))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Schema 
spells :: [Spell]
spells = [spell1,spell2,spell3]

-- type API = "spell" :> QueryParam "filterby" FilterBy :> Get '[JSON] [Spell]
-- :: Handler .. -> Server API
--
-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     pullSpells :: Text -> Maybe FilterBy -> m (Either SomeException [SpellToWeb])   --user name -> filter ->...
--   }

-- type AppM m = ReaderT (Handle m) Handler

serverSp :: (Monad m) => Handle m -> ServerT API (AppM m)
serverSp h' = getSpells
  where getSpells :: (Monad m) => Handle m -> Maybe FilterBy -> AppM m [SpellToWeb]
        getSpells h Nothing = do
          case Right undefined of
            Left e -> error "not"
            Right stw -> pure stw

  --       handler (Just NotApproved) = return (take 1 spells)
  --       handler (Just OwnSpells) = return (drop 2 spells)
  -- ei <- DA.pullSpells pginfo "user4" (Just OwnSpells)
--
-- nt :: (Monad m) => m -> AppM a -> Handler a
-- nt s x = runReaderT x s
--
--
-- spellServer :: (Monad m) => Handle m -> Application
-- spellServer h = serve @API Proxy $ serverSp  h

-- spellServer :: Application
-- spellServer = serve @API Proxy $ serverSp ()
-- serverSp :: Handle m -> Server API
-- serverSp h = handler h
--   where handler :: Handle m -> Maybe FilterBy -> Handler [SpellToWeb]
--         handler h Nothing = do
--           ei <- (pullSpells h) "" Nothing
--           case ei of
--             Left e -> error "not"
--             Right stw -> pure stw
--         -- handler (Just NotApproved) = return (take 1 spells)
--         -- handler (Just OwnSpells) = return (drop 2 spells)
--   -- Kjkgt
--   -- ei <- DA.pullSpells pginfo "user4" (Just OwnSpells)
