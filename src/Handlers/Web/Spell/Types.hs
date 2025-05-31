module Handlers.Web.Spell.Types  (SpellInternal(..),PhraseInternal(..)) where
import Data.Text (Text)
import Schema 
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
