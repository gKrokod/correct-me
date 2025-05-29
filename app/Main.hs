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
import qualified Logger
import Config

-- {"cHostDB":"127.0.0.1","cLogLvl":"Debug","cNameDB":"bobdb","cPasswordDB":"1","cPortDB":"5432","cPortServer":4221,"cUserDB":"bob"}
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
      handle =
        Handlers.Base.Handle
          { 
            Handlers.Base.logger = logHandle,
            Handlers.Base.pullSpells = DA.pullSpells pginfo
          }
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
  putStrLn "by"
  checkError

-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     pullSpells :: Text -> Maybe FilterBy -> m (Either SomeException [SpellToWeb])   --user name -> filter ->...
--   }
