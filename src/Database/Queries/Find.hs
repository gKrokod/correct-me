module Database.Queries.Find (findUserByName, findPhrase, findSpellById) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Database.Esqueleto.Experimental (get, getBy)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Schema (User, Phrase, Spell, Unique(..))
import Web.Types (Client (..), Id(..))

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

findSpellById :: ConnectionString -> Id -> IO (Either SomeException (Maybe Spell))
findSpellById connString (MkId uid) = try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m (Maybe Spell)
    fetchAction = get (toSqlKey uid)
