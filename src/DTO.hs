{-# LANGUAGE DuplicateRecordFields #-}

module DTO (PhraseToWeb(..), SpellToWeb(..))where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
-- import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import  Data.Text (Text)
import GHC.Generics (Generic)
import Schema 
import Data.Int (Int64)

data PhraseToWeb = PhraseToWeb
  { phrase :: Text,
    author :: Text,
    revision :: SpellResult
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data SpellToWeb = SpellToWeb
  { id :: Int64,
    phrase :: PhraseToWeb,
    paraphrases :: [PhraseToWeb],
    isApproved :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

