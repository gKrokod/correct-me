{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Web.Yandex as WA
import Database.Migrations.Migration (migrationEngine) 
import Config
import Database.Verb (runDataBaseWithOutLog)

import Data.Text.Encoding (encodeUtf8)
import Schema
import Database.Persist.Postgresql (ConnectionString)
import qualified Database.Api as DA
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
import Handlers.Router (doLogic, doAuthorization)
import Handlers.Web.Base (logger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import qualified Web.Utils as WU

-- main :: IO ()
-- main = do
--   config <- loadConfig
--   DA.migrationEngine (connectionString config)
--   serverSetup <- makeSetup config
--   run (cPortServer config) $ authorization serverSetup app
--
main :: IO ()
main = do
  putStrLn "hi"
  config <- loadConfig
  let pginfo = connectionString config
  migrationEngine pginfo 

  let logHandle =
         Handlers.Logger.Handle
           { Handlers.Logger.levelLogger = cLogLvl config,
             Handlers.Logger.writeLog = Logger.writeLog
          }
      baseSpellHandle =
         Handlers.Database.Spell.Handle
           { 
             Handlers.Database.Spell.logger = logHandle,
             Handlers.Database.Spell.pullSpells = DA.pullSpells pginfo,
             Handlers.Database.Spell.putSpell = DA.putSpell pginfo, 
             Handlers.Database.Spell.findUserByName = DA.findUserByName pginfo,
             Handlers.Database.Spell.findPhrase = DA.findPhrase pginfo,
             Handlers.Database.Spell.addPhrase = DA.addPhrase pginfo,
             Handlers.Database.Spell.findSpellById = DA.findSpellById pginfo,
             Handlers.Database.Spell.createUser = DA.createUser pginfo
          }
      spellHandle =
        Handlers.Web.Spell.Handle
          { Handlers.Web.Spell.logger = logHandle,
            Handlers.Web.Spell.base = baseSpellHandle,
            Handlers.Web.Spell.revisionSpell = WA.revisionSpell,
            Handlers.Web.Spell.getBody = WU.getBody
          }      
      handle =
        Handlers.Web.Base.Handle
          { Handlers.Web.Base.connectionString = pginfo,
            Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.spell = spellHandle,
            Handlers.Web.Base.client = Nothing
            
          }        

  run (cPortServer config) $ authorization handle app

type ServerSetup m = Handlers.Web.Base.Handle m

app :: ServerSetup IO -> Application
app h req f =
  bracket_
    (logMessage (logger h) Info "Open app")
    (logMessage (logger h) Info "Close app")
    (doLogic h req >>= f)

authorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
authorization h nextApp req respond = do
  check <- doAuthorization h req
  case check of
    Left fail' -> respond fail'
    Right h' -> nextApp h' req respond
