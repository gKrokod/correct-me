{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Add (addPhrase) where

import Control.Exception (SomeException, throw, try)
import Database.Esqueleto.Experimental (Key, get, getBy, insert, replace)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey)
import Database.Verb (runDataBaseWithOutLog)
import Handlers.Web.Spell.Types (PhraseInternal (..), SpellInternal (..))
import Schema
import Web.Types (Client (..))

addPhrase :: ConnectionString -> PhraseInternal -> IO (Either SomeException ())
addPhrase pginfo (PhraseInternal id (SpellInternal {..})) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        keyUser <- (fmap . fmap) entityKey (getBy . UniqueUserName . name $ author)
        let spellKey = toSqlKey id :: Key Spell
        maybeSpell <- get spellKey
        case (keyUser, maybeSpell) of
          (Just keyUsr, Just spell) -> do
            keySpelling <- insert $ Spelling {spellingRevisions = revision}
            keyPhrase <-
              insert $
                Phrase
                  { phraseText = phrase,
                    phraseUserId = keyUsr,
                    phraseSpellingId = keySpelling
                  }
            let updatedSpell = spell {spellParaphrasesId = keyPhrase : spellParaphrasesId spell}
            replace spellKey updatedSpell
          _ -> throw $ userError "function addPhrase"
    )
