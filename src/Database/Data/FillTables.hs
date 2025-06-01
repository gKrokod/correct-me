module Database.Data.FillTables (user1, user2, user3, user4, spelling1, spelling2, spelling3, spelling4, spelling5, spelling6, phrase1, phrase2, phrase3, phrase4, phrase5, phrase6, spell1, spell2, spell3) where

import Database.Persist.Postgresql (toSqlKey)
import qualified Schema as S
import Web.Types (SpellRevision (..))

user1, user2, user3, user4 :: S.User
user1 = S.User "user1"
user2 = S.User "user2"
user3 = S.User "user3"
user4 = S.User "user4"

spelling1, spelling2, spelling3, spelling4, spelling5, spelling6 :: S.Spelling
-- запрос: превет москва москва как твои дила
spelling1 =
  S.Spelling
    [ MkSpellRevision 1 0 0 0 6 "Превет" ["Привет", "Превед"],
      MkSpellRevision 1 21 0 21 3 "каг" ["как", "каг"],
      MkSpellRevision 1 30 0 30 4 "дила" ["дела", "дила"]
    ]
spelling2 = S.Spelling [MkSpellRevision 1 2 2 2 1 "word" ["word", "ward"], MkSpellRevision 4 3 3 3 3 "vo" ["vi", "vy"]]
spelling3 = S.Spelling []
spelling4 = S.Spelling []
spelling5 = S.Spelling []
spelling6 = S.Spelling []

phrase1, phrase2, phrase3, phrase4, phrase5, phrase6 :: S.Phrase
phrase1 = S.Phrase "превет москва москва как твои дила" (toSqlKey 1) (toSqlKey 1) -- user spelling
phrase2 = S.Phrase "second phrase word vo" (toSqlKey 2) (toSqlKey 2)
phrase3 = S.Phrase "third phrase" (toSqlKey 3) (toSqlKey 3)
phrase4 = S.Phrase "Angle" (toSqlKey 4) (toSqlKey 4)
phrase5 = S.Phrase "angle" (toSqlKey 2) (toSqlKey 5)
phrase6 = S.Phrase "Angless" (toSqlKey 3) (toSqlKey 6)

spell1, spell2, spell3 :: S.Spell
spell1 = S.Spell (toSqlKey 1) [] False
spell2 = S.Spell (toSqlKey 2) [toSqlKey 3] True
spell3 = S.Spell (toSqlKey 4) [toSqlKey 5, toSqlKey 6] False -- phraseId -- paraphrasesIs
