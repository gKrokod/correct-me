{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Check (checkSpell,validCheck)  where

import Control.Exception (SomeException, throw, try)
import Database.Esqueleto.Experimental ( get, Key,  getBy,  replace)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey)
import Database.Verb (runDataBaseWithOutLog)
import Schema 
import Handlers.Web.Spell.Types (CheckSpellInternal(..))

checkSpell :: ConnectionString -> CheckSpellInternal -> IO (Either SomeException ())   
checkSpell pginfo CheckSpellInternal {..} = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
            keyPhrase <- (fmap . fmap) entityKey (getBy . UniquePhraseText $ phrase )
            let spellKey = toSqlKey idSpell :: Key Spell
            maybeSpell <- get spellKey
            case (keyPhrase, maybeSpell) of
              (Just keyPhrase, Just spell) -> do
                let updatedSpell = spell {spellPhraseId = keyPhrase, spellIsApproved = True }
                replace spellKey updatedSpell
              _ -> throw $ userError "function checkSpell"
    )


validCheck :: ConnectionString -> CheckSpellInternal -> IO (Either SomeException Bool)   
validCheck pginfo CheckSpellInternal {..} = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
            keyPhrase <- (fmap . fmap) entityKey (getBy . UniquePhraseText $ phrase )
            let spellKey = toSqlKey idSpell :: Key Spell
            maybeSpell <- get spellKey
            case (keyPhrase, maybeSpell) of
              (Just keyPhrase, Just spell) -> pure $ elem keyPhrase $ (:) <$> spellPhraseId <*> spellParaphrasesId $ spell
              _ -> throw $ userError "function validChec"
    )
