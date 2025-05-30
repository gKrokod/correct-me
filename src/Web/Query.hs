{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Web.Query  where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import qualified Data.ByteString as B
import Data.ByteString.Base64 as B64
import Data.CaseInsensitive (CI)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)
import Schema 

