{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Create (putSpell, createUser)  where

import Control.Exception (SomeException, throw, try)
import Database.Persist.Postgresql (rawExecute, rawSql)
import Control.Monad.IO.Class (MonadIO)
import  Data.Text (Text)
import Database.Esqueleto.Experimental (keyToValues, get,valList, in_, Key, OrderBy, PersistField (..), SqlExpr, Value (..), asc, count, delete, desc, from, fromSqlKey, getBy, groupBy, innerJoin, insert, insertMany, insertMany_, just, leftJoin, like, limit, offset, on, orderBy, replace, select, table, unionAll_, val, where_, withRecursive, (%), (&&.), (++.), (:&) (..), (<.), (==.), (>=.), (?.), (^.), (||.),union_,subList_select, exists, insert_)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey, fromSqlKey)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Schema 
import Schema (Unique(..))
import Data.Int
import Web.DTO.Spell
import Handlers.Web.Spell.Types (SpellInternal (..))
import Web.Types(Client(..),FilterBy(..))

putSpell :: ConnectionString -> SpellInternal -> IO (Either SomeException ())   
putSpell pginfo (SpellInternal {..}) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ do
            keyUser <- (fmap . fmap) entityKey (getBy . UniqueUserName . name $ author)
            case keyUser of
              (Just keyUsr) -> do
                keySpelling <- insert $ Spelling { spellingRevisions = revision }
                keyPhrase <- insert $ Phrase
                      { phraseText = phrase,
                        phraseUserId = keyUsr,
                        phraseSpellingId = keySpelling
                      }
                insert_ $ Spell 
                  { spellPhraseId = keyPhrase,
                    spellParaphrasesId = [],
                    spellIsApproved = False
                  }
              _ -> throw $ userError "function putNews fail"
    )

createUser :: ConnectionString -> Client -> IO (Either SomeException ())   
createUser pginfo (Client name) = do
  try @SomeException
    ( runDataBaseWithOutLog pginfo $ insert_ $ User { userName = name }
    )

