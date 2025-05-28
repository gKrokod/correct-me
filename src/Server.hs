module Server () where

import Api
import Database.Data.FillTables  (spell1,spell2,spell3)
import Servant

spells :: [Spell]
spells = [spell1,spell2,spell3]

--handler сюда прикrутить свой
-- :: Handler .. -> Server API
serverSp :: () -> Server API
serverSp _ = handler
  where handler :: Maybe FilterBy -> Handler [Spell]
        handler Nothing = return spells
        handler (Just NotApproved) = return (take 1 spells)
        handler (Just OwnSpells) = return (drop 2 spells)

spellServer :: Application
spellServer = serve @API Proxy $ serverSp ()
