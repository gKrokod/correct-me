{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Add (addPhrase)  where

import Control.Exception (SomeException, throw, try)
import Database.Esqueleto.Experimental (keyToValues, get,valList, in_, Key, OrderBy, PersistField (..), SqlExpr, Value (..), asc, count, delete, desc, from, fromSqlKey, getBy, groupBy, innerJoin, insert, insertMany, insertMany_, just, leftJoin, like, limit, offset, on, orderBy, replace, select, table, unionAll_, val, where_, withRecursive, (%), (&&.), (++.), (:&) (..), (<.), (==.), (>=.), (?.), (^.), (||.),union_,subList_select, exists, insert_)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey, fromSqlKey)
import Database.Verb (runDataBaseWithOutLog)
import Schema 
import Handlers.Web.Spell.Types (SpellInternal (..), PhraseInternal(..))
import Web.Types(Client(..),FilterBy(..))

addPhrase :: ConnectionString -> PhraseInternal -> IO (Either SomeException ())   
addPhrase pginfo (PhraseInternal id (SpellInternal {..})) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
            keyUser <- (fmap . fmap) entityKey (getBy . UniqueUserName . name $ author)
            let spellKey = toSqlKey id :: Key Spell
            maybeSpell <- get spellKey
            case (keyUser, maybeSpell) of
              (Just keyUsr, Just spell) -> do
                keySpelling <- insert $ Spelling { spellingRevisions = revision }
                keyPhrase <- insert $ Phrase
                      { phraseText = phrase,
                        phraseUserId = keyUsr,
                        phraseSpellingId = keySpelling
                      }
                let updatedSpell = spell {spellParaphrasesId = keyPhrase : spellParaphrasesId spell }
                replace spellKey updatedSpell
              _ -> throw $ userError "function addPhrase"
    )


