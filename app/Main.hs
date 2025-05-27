{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Yandex (checkError)
import Control.Monad
import Server (spellServer)
import Network.Wai.Handler.Warp (run)
import Control.Exception
import Network.HTTP.Simple
-- import qualified Database.Api as DA
import Database.Migrations.Migration (migrationEngine) 


import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)
import Database.Persist.Postgresql (ConnectionString)

-- {"cHostDB":"127.0.0.1","cLogLvl":"Debug","cNameDB":"bobdb","cPasswordDB":"1","cPortDB":"5432","cPortServer":4221,"cUserDB":"bob"}
main :: IO ()
main = do
  print "hi"
  migrationEngine (connectionString )
  checkError
  print "by"

connectionString :: ConnectionString
connectionString =
  encodeUtf8 $
    mconcat
      [ "host=",
        "127.0.0.1",
        " port=",
        "5432",
        " user=",
        "bob",
        " dbname=",
        "bobdb",
        " password=",
        "1"
      ]
