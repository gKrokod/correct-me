{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Yandex (checkError)
-- import qualified Database.Api as DA
import Database.Migrations.Migration (migrationEngine) 
import Database.Queries.Spell
import Config
import Database.Verb (runDataBaseWithOutLog)

import Data.Text.Encoding (encodeUtf8)
import Schema
import Database.Persist.Postgresql (ConnectionString)
import qualified Database.Api as DA
import qualified Handlers.Base
import qualified Handlers.Logger
import qualified Handlers.Database.Spell
import Handlers.Logger
import qualified Handlers.Web.Base
import qualified Handlers.Web.Spell

import qualified Logger
import Config
import Control.Exception (bracket_)
import qualified Database.Api as DA
import Handlers.Logger (Log (Info), logMessage)
import Handlers.Router (doLogic)
import Handlers.Web.Base (logger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Web.Utils as WU

-- {"cHostDB":"127.0.0.1","cLogLvl":"Debug","cNameDB":"bobdb","cPasswordDB":"1","cPortDB":"5432","cPortServer":4221,"cUserDB":"bob"}
--type ServerSetup m = Handlers.Web.Base.Handle m

main :: IO ()
main = do
  putStrLn "hi"
  config <- loadConfig
  let pginfo = connectionString config
  migrationEngine pginfo 

-- data Handle m = Handle
--   { connectionString :: ConnectionString,
--     logger :: Handlers.Logger.Handle m,
--     spell :: Handlers.Web.Spell.Handle m, -- base this,
--     revisionSpell :: Text -> m Bool
--     -- client :: Handlers.Service.Yandex.Client m -- tyt clienta potom
--   }

  let logHandle =
         Handlers.Logger.Handle
           { Handlers.Logger.levelLogger = cLogLvl config,
             Handlers.Logger.writeLog = Logger.writeLog
          }
      baseSpellHandle =
         Handlers.Database.Spell.Handle
           { 
             Handlers.Database.Spell.logger = logHandle,
             Handlers.Database.Spell.pullSpells = DA.pullSpells pginfo
          }
      spellHandle =
        Handlers.Web.Spell.Handle
          { Handlers.Web.Spell.logger = logHandle,
            Handlers.Web.Spell.base = baseSpellHandle,
            Handlers.Web.Spell.getBody = WU.getBody
          }      
      handle =
        Handlers.Web.Base.Handle
          { Handlers.Web.Base.connectionString = pginfo,
            Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.spell = spellHandle,
            Handlers.Web.Base.revisionSpell = \_ -> pure True
          }        

-- data Handle m = Handle
--   { connectionString :: ConnectionString,
--     logger :: Handlers.Logger.Handle m,
--     spell :: Handlers.Web.Spell.Handle m, -- base this,
--     revisionSpell :: Text -> m Bool
--     -- client :: Handlers.Service.Yandex.Client m -- tyt clienta potom
--   }
  run (cPortServer config) $ app handle

-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     base :: Handlers.Database.Spell.Handle m,
--     getBody :: Request -> m B.ByteString
--   }
-- main :: IO ()
-- main = do
--   config <- loadConfig
--   DA.migrationEngine (connectionString config)
--   serverSetup <- makeSetup config
--   run (cPortServer config) $ authorization serverSetup app

app :: Handlers.Web.Base.Handle IO -> Application
app h req f =
  bracket_
    (logMessage (logger h) Info "Open app")
    (logMessage (logger h) Info "Close app")
    (doLogic h req >>= f)

-- authorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
-- authorization h nextApp req respond = do
--   check <- doAuthorization h req
--   case check of
--     Left fail' -> respond fail'
--     Right h' -> nextApp h' req respond

  -- ei <- Handlers.Base.pullSpells handle "user1" Nothing
  -- ei <- pullAllUsers pginfo
  -- putStrLn "***user****************************"
  -- mapM_ print ei
  -- putStrLn "******prase*************************"
  -- ei <- pullAllPh pginfo
  -- mapM_ print ei
  -- putStrLn "**********spelling*********************"
  -- ei <- pullAllSl pginfo
  -- mapM_ print ei
  -- putStrLn "**********fetch all*********************"
  -- ei <- 
  --  (runDataBaseWithOutLog pginfo (fetchAllz))
  -- mapM print ei
  -- checkError
  -- putStrLn "**********spells*********************"
  -- ei <- DA.pullSpells pginfo "" Nothing 
  -- case ei of
  --   Left _ -> print "left"
  --   Right x -> mapM_ (\x -> print "" >> print x) x
  -- putStrLn "**********spells Own user1*********************"
  -- ei <- DA.pullSpells pginfo "user4" (Just OwnSpells)
  -- case ei of
  --   Left _ -> print "left"
  --   Right x -> mapM_ (\x -> print "" >> print x) x
  -- putStrLn "**********spells NotApproved*********************"
  -- ei <- DA.pullSpells pginfo "user3" (Just NotApproved)
  -- case ei of
  --   Left _ -> print "left"
  --   Right x -> mapM_ (\x -> print "" >> print x) x

-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     pullSpells :: Text -> Maybe FilterBy -> m (Either SomeException [SpellToWeb])   --user name -> filter ->...
--   }
