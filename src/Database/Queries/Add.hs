{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Add (addPhrase)  where

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
import Handlers.Web.Spell.Types (SpellInternal (..), PhraseInternal(..))
import Web.Types(Client(..),FilterBy(..))

addPhrase :: ConnectionString -> PhraseInternal -> IO (Either SomeException ())   
addPhrase pginfo (PhraseInternal id (SpellInternal {..})) = do
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
                pure () -- todo phrase vstavil, ter' nado v spell ee razmestit
                -- insert_ $ Spell 
                --   { spellPhraseId = keyPhrase,
                --     spellParaphrasesId = [],
                --     spellIsApproved = False
                --   }
              _ -> throw $ userError "function putNews fail"
    )


