module Database.Data.FillTables  where

import qualified Database.Migrations.Migrationv0 as S
import Database.Persist.Postgresql (toSqlKey)
import Yandex
-- import qualified Schema as S

user1, user2, user3, user4 :: S.User
user1 = S.User "user1"
user2 = S.User "user2"
user3 = S.User "user3"
user4 = S.User "user4"

spe1, spe2 :: S.Spelling 
-- превет москва москва как твои дила
spe1 = S.Spelling 
  [MkSpellError 1 0 0 0 6 "Превет" ["Привет", "Превед"] ,
  MkSpellError 1 21 0 21 3 "каг" ["как", "каг"] ,
  MkSpellError 1 30 0 30 4 "дила" ["дела", "дила"]]
spe2 = S.Spelling [MkSpellError 1 2 2 2 1 "word" ["word","ward"], MkSpellError 4 3 3 3 3 "vo" ["vi","vy"]]


  -- [MkSpellError 1, 0, 0, 0, 6, "Превет", [ "Привет", "Превед" ] ,
  -- MkSpellError 1, 21, 0, 21, 3, "каг", [ "как", "каг" ] ,
  -- MkSpellError 1, 30, 0, 30, 4, "дила", [ "дела", "дила" ]]

phrase1,phrase2 :: S.Phrase
phrase1 = S.Phrase "превет москва москва как твои дила" (toSqlKey 1) (toSqlKey 1)
phrase2 = S.Phrase "second phrase" (toSqlKey 2) (toSqlKey 2)
phrase3 = S.Phrase "third phrase" (toSqlKey 3) (toSqlKey 1)


spell1,spell2 :: S.Spell
spell1 = S.Spell (toSqlKey 1) False
spell2 = S.Spell (toSqlKey 2) True
