{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Create (putSpell, createUser) where

import Control.Exception (SomeException, throw, try)
import Database.Esqueleto.Experimental (getBy, insert, insert_)
import Database.Persist.Postgresql (ConnectionString, Entity (..))
import Database.Verb (runDataBaseWithOutLog)
import Handlers.Web.Spell.Types (SpellInternal (..))
import Schema
import Web.Types (Client (..))

putSpell :: ConnectionString -> SpellInternal -> IO (Either SomeException ())
putSpell pginfo (SpellInternal {..}) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
        keyUser <- (fmap . fmap) entityKey (getBy . UniqueUserName . name $ author)
        case keyUser of
          (Just keyUsr) -> do
            keySpelling <- insert $ Spelling {spellingRevisions = revision}
            keyPhrase <-
              insert $
                Phrase
                  { phraseText = phrase,
                    phraseUserId = keyUsr,
                    phraseSpellingId = keySpelling
                  }
            insert_ $
              Spell
                { spellPhraseId = keyPhrase,
                  spellParaphrasesId = [],
                  spellIsApproved = False
                }
          _ -> throw $ userError "function putNews fail"
    )

createUser :: ConnectionString -> Client -> IO (Either SomeException ())
createUser pginfo (Client name) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ insert_ $ User {userName = name}
    )
