{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Server (startApp)
-- import qualified Data.Text as T
import Data.Text (Text, toLower)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Servant
import Data.Time.Calendar
import Network.Wai.Handler.Warp (run)

-- autorization add
type API = "spell" :> QueryParam "filterby" FilterBy :> Get '[JSON] [Spell]


data Sp = Sp {
  spellText :: Text, --(approved?) -- spell = Main Text [Text]
  isApproved :: Bool,
  author :: Text, -- UserName
  var :: (Text, [(Text,[Text])]) -- (phrase [(Word of phrase, [Maybe Word)])
             }

-- spell = {text, id, author, isApproved, errors, variantSpell}
-- veriantSpell = {text,author, errors}
--
--
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
data Spell = Spell Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FilterBy = NotApproved | OwnSpells
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance FromHttpApiData FilterBy where
  parseQueryParam txt = case toLower txt of
                          "notapproved" -> Right NotApproved
                          "ownspells" -> Right OwnSpells
                          _ -> Left "Invalid SortBy value (agreed or author) <- FromHttpApiData instance"

instance ToHttpApiData FilterBy where
  toQueryParam NotApproved = "notapproved" 
  toQueryParam OwnSpells = "ownspells" 


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

  -- startApp
main :: IO ()
main = do
  --handler <<- loadConfig, DataBase, Setup another
  run 8081 . serve @API Proxy $ serverSp ()

