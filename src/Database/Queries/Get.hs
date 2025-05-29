{-# LANGUAGE RecordWildCards #-}

module Database.Queries.Get (pullSpells)  where

import Control.Exception (SomeException, throw, try)
import Database.Persist.Postgresql (rawExecute, rawSql)
import Control.Monad.IO.Class (MonadIO)
import  Data.Text (Text)
import Database.Esqueleto.Experimental (keyToValues, get,valList, in_, Key, OrderBy, PersistField (..), SqlExpr, Value (..), asc, count, delete, desc, from, fromSqlKey, getBy, groupBy, innerJoin, insert, insertMany, insertMany_, just, leftJoin, like, limit, offset, on, orderBy, replace, select, table, unionAll_, val, where_, withRecursive, (%), (&&.), (++.), (:&) (..), (<.), (==.), (>=.), (?.), (^.), (||.),union_,subList_select, exists)
import Database.Persist.Postgresql (ConnectionString, Entity (..), toSqlKey, fromSqlKey)
import Database.Persist.Sql (SqlPersistT)
import Database.Verb (runDataBaseWithOutLog)
import Schema 
import Data.Int
import DTO


pullSpells :: ConnectionString -> Text -> Maybe FilterBy -> IO (Either SomeException [SpellToWeb])   --user name -> filter ->...
pullSpells connString author mbFilter = do
  try @SomeException (runDataBaseWithOutLog connString fetchAction)
    where
      fetchAction :: (MonadFail m, MonadIO m) => SqlPersistT m [SpellToWeb]
      fetchAction = do
        s <- select $ do
            (spells :& phrases :& users :& spelling) <- 
              from $ table @Spell
                `innerJoin` table @Phrase 
                  `on` (\(s :& p) -> s ^. SpellPhraseId ==. p ^. PhraseId)
                `innerJoin` table @User 
                  `on` (\(_ :& p :& u) -> p ^. PhraseUserId ==. u ^. UserId)
                `innerJoin` table @Spelling 
                  `on` (\(_ :& p :& _ :& sp) -> p ^. PhraseSpellingId ==. sp ^. SpellingId)
            groupBy (spells ^. SpellId, phrases ^. PhraseText, users ^. UserId, spelling ^. SpellingRevisions) 

            maybe
                (where_ (val True))
                (\case
                    OwnSpells -> where_ (users ^. UserName ==. val author)
                    NotApproved -> where_ (spells ^. SpellIsApproved ==. val False)   
                    -- OwnAndNot -> where_ ( (users ^. UserName ==. val author)
                    --                       ||.(spells ^. SpellIsApproved ==. val False)
                    --                     )   
                )
                mbFilter
            pure spells

        mapM f s
          where   
                f entity = do
                  (main : _) <- fetchFullPhrase (spellPhraseId $ entityVal entity)
                  variants <- concat <$> mapM fetchFullPhrase (spellParaphrasesId $ entityVal entity)
                  let result =
                        SpellToWeb {
                          id = fromSqlKey $ entityKey entity,
                          phrase = main,
                          paraphrases = variants,
                          isApproved = spellIsApproved $ entityVal entity
                                    }
                  pure result

                fetchFullPhrase :: (MonadFail m, MonadIO m) => Key Phrase -> SqlPersistT m [PhraseToWeb]
                fetchFullPhrase keyP = do
                  ((p,u,s) : _) <- select $ do
                      (phrases :& users :& spelling) <- 
                        from $ table @Phrase
                          `innerJoin` table @User 
                            `on` (\(p :& u) -> p ^. PhraseUserId ==. u ^. UserId)
                          `innerJoin` table @Spelling 
                            `on` (\(p :& _ :& sp) -> p ^. PhraseSpellingId ==. sp ^. SpellingId)
                      groupBy (phrases ^. PhraseText, users ^. UserId, spelling ^. SpellingRevisions) 
                      where_ (phrases ^. PhraseId ==. val keyP)
                      pure (phrases ^. PhraseText, users ^. UserName, spelling ^. SpellingRevisions)

                  let result =
                        PhraseToWeb {
                          phrase = unValue p,
                          author = unValue u,
                          revision = unValue s
                                    }
                  pure [result]

