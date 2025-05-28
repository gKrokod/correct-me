{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Yandex (checkError)
-- import qualified Database.Api as DA
import Database.Migrations.Migration (migrationEngine) 


import Data.Text.Encoding (encodeUtf8)

import Database.Persist.Postgresql (ConnectionString)

-- {"cHostDB":"127.0.0.1","cLogLvl":"Debug","cNameDB":"bobdb","cPasswordDB":"1","cPortDB":"5432","cPortServer":4221,"cUserDB":"bob"}
main :: IO ()
main = do
  putStrLn "hi"
  migrationEngine (connectionString )
  checkError
  putStrLn "by"

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
