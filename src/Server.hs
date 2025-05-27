{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleContexts #-}
module Server where

import Api

import Servant
import Network.Wai.Handler.Warp (run)

-- on yandex {}
-- слюжба занятости
-- вада
-- вариант нармы
--[
--   {
--     "code": 1,
--     "pos": 0,
--     "row": 0,
--     "col": 0,
--     "len": 6,
--     "word": "слюжба",
--     "s": [
--       "служба",
--       "службы",
--       "службе",
--       "службу"
--     ]
--   },
--   {
--     "code": 1,
--     "pos": 32,
--     "row": 2,
--     "col": 8,
--     "len": 5,
--     "word": "нармы",
--     "s": [
--       "нормы"
--     ]
--   }
-- ]


spells :: [Spell]
spells = [Spell "one", Spell "two", Spell "three"]


--handler сюда прикrутить свой
-- :: Handler .. -> Server API
serverSp :: () -> Server API
serverSp _ = handler
  where handler :: Maybe FilterBy -> Handler [Spell]
        handler Nothing = return spells
        handler (Just NotApproved) = return (take 1 spells)
        handler (Just OwnSpells) = return (drop 2 spells)

spellServer = serve @API Proxy $ serverSp ()

-- on yandex {}
-- слюжба занятости
-- вада
-- вариант нармы
--[
--   {
--     "code": 1,
--     "pos": 0,
--     "row": 0,
--     "col": 0,
--     "len": 6,
--     "word": "слюжба",
--     "s": [
--       "служба",
--       "службы",
--       "службе",
--       "службу"
--     ]
--   },
--   {
--     "code": 1,
--     "pos": 32,
--     "row": 2,
--     "col": 8,
--     "len": 5,
--     "word": "нармы",
--     "s": [
--       "нормы"
--     ]
--   }
-- ]
