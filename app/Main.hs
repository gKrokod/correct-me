{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Yandex (checkError)
import Control.Monad
import Server (spellServer)
import Network.Wai.Handler.Warp (run)
import Control.Exception
import Network.HTTP.Simple

import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)

main :: IO ()
main = do
  print "hi"
  checkError
