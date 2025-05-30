{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Find (findUserByName, findPhrase, findSpellById)  where

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

findUserByName :: ConnectionString -> Client -> IO (Either SomeException (Maybe User))
findUserByName connString author = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe User)
    fetchAction = (fmap . fmap) entityVal (getBy . UniqueUserName . name $ author)

findPhrase :: ConnectionString -> Text -> IO (Either SomeException (Maybe Phrase))
findPhrase connString phrase = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Phrase)
    fetchAction = (fmap . fmap) entityVal (getBy $ UniquePhraseText phrase)

findSpellById :: ConnectionString -> Int64 -> IO (Either SomeException (Maybe Spell))
findSpellById connString uid = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Spell)
    fetchAction = get (toSqlKey uid)
