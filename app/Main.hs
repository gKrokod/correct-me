{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Yandex (checkError)
-- import qualified Database.Api as DA
import Database.Migrations.Migration (migrationEngine) 
import Config

import Data.Text.Encoding (encodeUtf8)

import Database.Persist.Postgresql (ConnectionString)

-- {"cHostDB":"127.0.0.1","cLogLvl":"Debug","cNameDB":"bobdb","cPasswordDB":"1","cPortDB":"5432","cPortServer":4221,"cUserDB":"bob"}
main :: IO ()
main = do
  putStrLn "hi"
  config <- loadConfig
  migrationEngine (connectionString config)
  checkError
  putStrLn "by"

