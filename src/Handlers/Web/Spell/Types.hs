{-# LANGUAGE DuplicateRecordFields #-}

module Handlers.Web.Spell.Types (SpellInternal (..), PhraseInternal (..), CheckSpellInternal (..)) where

import Data.Text (Text)
import Web.Types (Client, SpellResult, Id)

data SpellInternal = SpellInternal
  { phrase :: Text,
    author :: Client,
    revision :: SpellResult
  }

data PhraseInternal = PhraseInternal
  { idSpell :: Id,
    anotherPhrase :: SpellInternal
  }

data CheckSpellInternal = CheckSpellInternal
  { idSpell :: Id,
    client :: Client,
    phrase :: Text
  }
