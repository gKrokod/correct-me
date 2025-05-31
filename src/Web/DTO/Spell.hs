{-# LANGUAGE DuplicateRecordFields #-}

module Web.DTO.Spell (spellsToWeb, PhraseToWeb(..), SpellToWeb(..), webToPhrase, PhraseFromWeb(..), AnotherPhraseFromWeb(..),webToAnotherPhrase) where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.Binary.Builder (Builder, fromLazyByteString)
import qualified Data.ByteString as B
import  Data.Text (Text)
import GHC.Generics (Generic)
import Data.Int (Int64)
import Web.Types (SpellResult)

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

spellsToWeb :: [SpellToWeb] -> Builder
spellsToWeb = fromLazyByteString . encode @[SpellToWeb] 

newtype PhraseFromWeb = PhraseFromWeb
  { phrase :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToPhrase :: B.ByteString -> Either String PhraseFromWeb
webToPhrase = eitherDecodeStrict @PhraseFromWeb

data AnotherPhraseFromWeb = AnotherPhraseFromWeb
  { id :: Int64,
    phrase :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

webToAnotherPhrase :: B.ByteString -> Either String AnotherPhraseFromWeb
webToAnotherPhrase = eitherDecodeStrict @AnotherPhraseFromWeb
