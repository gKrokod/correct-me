{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Server (spellServer)
import Servant
import Network.Wai.Handler.Warp (run)

  -- startApp
main :: IO ()
main = do
  --handler <<- loadConfig, DataBase, Setup another
  run 8081 $ spellServer

