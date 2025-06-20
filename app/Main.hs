module Main (main) where

import Config (ConfigDataBase (..), ServerSetup, connectionString, loadConfigDataBase, loadConfigService, makeSetup)
import Control.Exception (bracket_)
import qualified Database.Api as DA
import Handlers.Logger (Log (Info), logMessage)
import Handlers.Router (doAuthorization, doLogic)
import Handlers.Web.Base (logger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  configDataBase <- loadConfigDataBase
  configService <- loadConfigService
  DA.migrationEngine (connectionString configDataBase)
  serverSetup <- makeSetup configDataBase configService
  run (cPortServer configDataBase) $ authorization serverSetup app

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
