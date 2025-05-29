{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Queries.Spell  where

import Control.Exception (SomeException, throw, try)
import Database.Persist.Postgresql (rawExecute, rawSql)
import Control.Monad.IO.Class (MonadIO)
import  Data.Text (Text)
-- import Data.Time (UTCTime (..), addDays)
import Database.Esqueleto.Experimental (keyToValues, get,valList, in_, Key, OrderBy, PersistField (..), SqlExpr, Value (..), asc, count, delete, desc, from, fromSqlKey, getBy, groupBy, innerJoin, insert, insertMany, insertMany_, just, leftJoin, like, limit, offset, on, orderBy, replace, select, table, unionAll_, val, where_, withRecursive, (%), (&&.), (++.), (:&) (..), (<.), (==.), (>=.), (?.), (^.), (||.),union_,subList_select, exists)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey, fromSqlKey)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
-- import Handlers.Database.Base (Limit (..), Offset (..), Success (..))
-- import Handlers.Web.Base (NewsEditInternal (..), NewsInternal (..), NewsOut (..))
import Schema 
import Data.Int
import DTO
-- import Types (Content (..), Label (..), Login (..), Name (..), Title (..), URI_Image (..))
-- import Database.Migrations.Migrationv0 


pullSpells :: ConnectionString -> Text -> Maybe FilterBy -> IO (Either SomeException [SpellToWeb])   --user name -> filter ->...
pullSpells connString author mbFilter = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
    where
      fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m [SpellToWeb]
      fetchAction = pure []
--

pullAllUsers :: ConnectionString -> IO (Either SomeException [User])
pullAllUsers connString = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [User]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            users <- from $ table @User
            -- offset (fromIntegral . getOffset $ userOffset)
            -- limit (fromIntegral . min configLimit . getLimit $ userLimit)
            pure users
        )

pullAllPh :: ConnectionString -> IO (Either SomeException [Phrase])
pullAllPh connString = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Phrase]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            phrases <- from $ table @Phrase
            -- offset (fromIntegral . getOffset $ userOffset)
            -- limit (fromIntegral . min configLimit . getLimit $ userLimit)
            pure phrases
        )

pullAllSl :: ConnectionString -> IO (Either SomeException [Spelling])
pullAllSl connString = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Spelling]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            spellings <- from $ table @Spelling
            -- offset (fromIntegral . getOffset $ userOffset)
            -- limit (fromIntegral . min configLimit . getLimit $ userLimit)
            pure spellings
        )

pullAllSpells :: ConnectionString -> IO (Either SomeException [Spell])
pullAllSpells connString = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Spell]
    fetchAction =
      (fmap . fmap)
        entityVal
        ( select $ do
            spells <- from $ table @Spell
            -- offset (fromIntegral . getOffset $ userOffset)
            -- limit (fromIntegral . min configLimit . getLimit $ userLimit)
            pure spells
        )

    --
fetchUser :: (MonadIO m) => SqlPersistT m [Entity User]
fetchUser = select $ do
  -- Основной JOIN для Spell и связанных сущностей
  users <- from $ table @User
  where_ (users ^. UserName ==. val "user1")
  pure users

fetchPhrase :: (MonadIO m) => SqlPersistT m [Entity Phrase]
fetchPhrase = select $ do
  -- Основной JOIN для Spell и связанных сущностей
  users <- from $ table @Phrase
  where_ (users ^. PhraseId ==. val (toSqlKey 1))
  pure users


--vse polya bez paraphraz
fetch1 :: (MonadIO m) => SqlPersistT m [(Value (Key Spell), Value Text, Value Text, Value [SpellRevision])]
fetch1 = select $ do
  (spells :& phrases :& users :& spelling) <- 
    from $ table @Spell
      `innerJoin` table @Phrase 
        -- `on` (\(s :& p) -> p ^. PhraseId `in_` valList (map toSqlKey [6]))
        `on` (\(s :& p) -> s ^. SpellPhraseId ==. p ^. PhraseId)
      `innerJoin` table @User 
        `on` (\(_ :& p :& u) -> p ^. PhraseUserId ==. u ^. UserId)
      `innerJoin` table @Spelling 
        `on` (\(_ :& p :& _ :& sp) -> p ^. PhraseSpellingId ==. sp ^. SpellingId)
  groupBy (spells ^. SpellId, phrases ^. PhraseText, users ^. UserId, spelling ^. SpellingRevisions) 
  -- where_ (phrases ^. PhraseId `in_` spells ^. SpellParaphrasesId))
  -- where_ (spells ^. SpellId ==. val (toSqlKey 1))
  pure (spells ^. SpellId, phrases ^. PhraseText, users ^. UserName, spelling ^. SpellingRevisions)

--key paraprhas
fetch2 :: (MonadIO m) => SqlPersistT m [Value [Key Phrase]]
fetch2 = select $ do
  (spells :& phrases :& users :& spelling) <- 
    from $ table @Spell
      `innerJoin` table @Phrase 
        `on` (\(s :& p) -> s ^. SpellPhraseId ==. p ^. PhraseId)
      `innerJoin` table @User 
        `on` (\(_ :& p :& u) -> p ^. PhraseUserId ==. u ^. UserId)
      `innerJoin` table @Spelling 
        `on` (\(_ :& p :& _ :& sp) -> p ^. PhraseSpellingId ==. sp ^. SpellingId)
  groupBy (spells ^. SpellId, phrases ^. PhraseText, users ^. UserId, spelling ^. SpellingRevisions) 
  pure (spells ^. SpellParaphrasesId)

-- fetchA :: (MonadIO m) => SqlPersistT m [Value [Key Phrase]]
fetchA :: (MonadIO m) => SqlPersistT m [[Key Phrase]]
-- fetchA :: (MonadIO m) => SqlPersistT m [[Int64]]
fetchA =
  (fmap . fmap) (unValue) (select $ do
  -- (fmap . fmap) (map fromSqlKey . unValue) (select $ do
        spells <- from $ table @Spell
        pure (spells ^. SpellParaphrasesId)
    )

fetchB :: (MonadIO m) => Key Phrase -> SqlPersistT m [(Value Text, Value Text, Value [SpellRevision])]
-- fetchB :: (MonadIO m) => Int64 -> SqlPersistT m [(Value Text, Value Text, Value [SpellRevision])]
fetchB keyP = select $ do
  (phrases :& users :& spelling) <- 
    from $ table @Phrase
      `innerJoin` table @User 
        `on` (\(p :& u) -> p ^. PhraseUserId ==. u ^. UserId)
      `innerJoin` table @Spelling 
        `on` (\(p :& _ :& sp) -> p ^. PhraseSpellingId ==. sp ^. SpellingId)
  groupBy (phrases ^. PhraseText, users ^. UserId, spelling ^. SpellingRevisions) 
  where_ (phrases ^. PhraseId ==. val keyP)
  -- where_ (phrases ^. PhraseId ==. val (toSqlKey keyP))
  pure (phrases ^. PhraseText, users ^. UserName, spelling ^. SpellingRevisions)


fetchAB :: (MonadFail m, MonadIO m) => SqlPersistT m [[[(Value Text, Value Text, Value [SpellRevision])]]]
fetchAB = do
  id <- fetchA
  mapM (mapM fetchB) id 
  

fetch5 :: (MonadIO m) => SqlPersistT m [Entity Phrase]
fetch5 = select $ do
  phrasesFromDirect <- do
    (spell :& phrase) <- from $ 
      table @Spell `innerJoin` table @Phrase
        `on` (\(s :& p) -> s ^. SpellPhraseId ==. p ^. PhraseId)
    return phrase

  phrasesAwayDirect <- do
    (spell :& phrase) <- from $ 
      table @Spell `innerJoin` table @Phrase
        `on` (\(s :& p) -> s ^. SpellPhraseId ==. p ^. PhraseId)
    return phrase

  return $ phrasesFromDirect 



