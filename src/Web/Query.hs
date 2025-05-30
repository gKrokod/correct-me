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


data FilterBy = NotApproved | OwnSpells -- OwnSpells and NotApproved?
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype FilterFromWeb = FilterFromWeb (Maybe FilterBy)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
--
queryToFilter :: [(B.ByteString, Maybe B.ByteString)] -> Maybe FilterBy
queryToFilter = convertFromWeb . mapMaybe (\(x, y) -> if x == "filter" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> Maybe FilterBy
    convertFromWeb [xs] = case eitherDecodeStrict @FilterFromWeb xs of
      Right (FilterFromWeb (Just x)) -> Just x
      _ -> Nothing
    convertFromWeb _ = Nothing
