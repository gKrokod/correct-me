-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DerivingStrategies #-}
--
module Config (makeSetup, loadConfigDataBase, connectionString, ServerSetup, ConfigDataBase(..), loadConfigService) where

import Web.Yandex (ConfigYandexService(..), revisionSpell)
import Control.Exception (SomeException, displayException, throwIO, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode)
import qualified Data.ByteString.Lazy as L
import GHC.Generics (Generic)
import Data.Text (Text)
import  Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (ConnectionString)
import qualified Database.Api as DA
import qualified Handlers.Database.Spell
import Handlers.Logger
import qualified Handlers.Web.Base
import qualified Handlers.Web.Spell
import qualified Logger
import qualified Web.Utils as WU

data ConfigDataBase = MkConfigDataBase
  { cHostDB :: Text,
    cPortDB :: Text,
    cUserDB :: Text,
    cNameDB :: Text,
    cPasswordDB :: Text,
    cPortServer :: Int,
    cLogLvl :: Log
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


loadConfigDB :: IO (Either String ConfigDataBase)
loadConfigDB =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (L.readFile "config/db.cfg")

loadConfigDataBase :: IO ConfigDataBase
loadConfigDataBase = do
  cfg <- loadConfigDB
  case cfg of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

loadConfigYandexService :: IO (Either String ConfigYandexService)
loadConfigYandexService =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (L.readFile "config/spellService.config" )

loadConfigService :: IO ConfigYandexService
loadConfigService = do
  cfg <- loadConfigYandexService
  case cfg of
    Left error' -> throwIO $ userError error'
    Right config -> pure config

connectionString :: ConfigDataBase -> ConnectionString
connectionString cfg =
  encodeUtf8 $
    mconcat
      [ "host=",
        cHostDB cfg,
        " port=",
        cPortDB cfg,
        " user=",
        cUserDB cfg,
        " dbname=",
        cNameDB cfg,
        " password=",
        cPasswordDB cfg
      ]

-- -- -- for testing
-- createConfigFile :: IO ()
-- createConfigFile = do
--   let testConfig = MkConfigDataBase {
--       cHostDB = "127.0.0.1"
--     , cPortDB = "5432"
--     , cUserDB = "bob"
--     , cNameDB = "bobdb"
--     , cPasswordDB = "1"
--     , cPortServer = 4221
--     , cLogLvl = Debug
--   }
--   let configToJSON = encode testConfig :: L.ByteString
--   L.writeFile "config/db.cfg" configToJSON
--
-- createConfigYandexFile :: IO ()
-- createConfigYandexFile = do
--   let testConfig = MkConfigYandexService {
--       cHost = "speller.yandex.net",
--     cPath = "/services/spellservice.json/checkText",
--     cPort  = 443,
--     cMethod = "GET",
--     cSecure = True
--   }
--   let configToJSON = encode testConfig :: L.ByteString
--   L.writeFile "config/spellService.config" configToJSON


type ServerSetup m = Handlers.Web.Base.Handle m

makeSetup :: ConfigDataBase -> ConfigYandexService -> IO (ServerSetup IO)
makeSetup cfgDB cfgServ = do
  let pginfo = connectionString cfgDB
  let logHandle =
         Handlers.Logger.Handle
           { Handlers.Logger.levelLogger = cLogLvl cfgDB,
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
             Handlers.Database.Spell.checkSpell = DA.checkSpell pginfo,
             Handlers.Database.Spell.validCheck = DA.validCheck pginfo,
             Handlers.Database.Spell.addPhrase = DA.addPhrase pginfo,
             Handlers.Database.Spell.findSpellById = DA.findSpellById pginfo,
             Handlers.Database.Spell.createUser = DA.createUser pginfo
          }
      spellHandle =
        Handlers.Web.Spell.Handle
          { Handlers.Web.Spell.logger = logHandle,
            Handlers.Web.Spell.base = baseSpellHandle,
            Handlers.Web.Spell.revisionSpell = revisionSpell cfgServ,
            Handlers.Web.Spell.getBody = WU.getBody
          }      
      handle =
        Handlers.Web.Base.Handle
          { Handlers.Web.Base.connectionString = pginfo,
            Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.spell = spellHandle,
            Handlers.Web.Base.client = Nothing
          }        
  pure handle
