{-# LANGUAGE DuplicateRecordFields #-}

module Handlers.Web.Spell.Types (SpellInternal (..), PhraseInternal (..), CheckSpellInternal (..)) where
import Web.Types (Client, SpellResult, Id, TextPhrase)

data SpellInternal = SpellInternal
  { phrase :: TextPhrase,
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
    phrase :: TextPhrase
  }
