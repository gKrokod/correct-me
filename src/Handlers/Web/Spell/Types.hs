{-# LANGUAGE DuplicateRecordFields #-}
module Handlers.Web.Spell.Types  (SpellInternal(..),PhraseInternal(..), CheckSpellInternal(..)) where
import Data.Text (Text)
import Web.Types(Client, SpellResult)
import Data.Int (Int64)

data SpellInternal = SpellInternal
  { phrase :: Text,
    author :: Client,
    revision :: SpellResult
  }

data PhraseInternal = PhraseInternal
  { idSpell :: Int64,
    anotherPhrase :: SpellInternal
  }

data CheckSpellInternal = CheckSpellInternal
  { idSpell :: Int64,
    client :: Client,
    phrase :: Text
  }
