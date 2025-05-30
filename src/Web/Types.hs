module Web.Types (Client(..), FilterBy(..), FilterFromWeb(..), SpellRevision(..),SpellResult(..)) where
import Data.Text (Text)  
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import qualified Data.ByteString as B
import Data.ByteString.Base64 as B64
import Data.CaseInsensitive (CI)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)

newtype Client = Client {name :: Text}

data FilterBy = NotApproved | OwnSpells -- OwnSpells and NotApproved?
  deriving stock (Show, Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

newtype FilterFromWeb = FilterFromWeb (Maybe FilterBy)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SpellRevision = MkSpellRevision {
  code :: Int,   -- code — код ошибки
  pos :: Int,    -- pos — позиция слова с ошибкой (отсчет от 0)
  row :: Int,    -- row — номер строки (отсчет от 0)
  col :: Int,    -- col — номер столбца (отсчет оn 0)
  len :: Int,    -- len — длина слова с ошибкой.
  word :: Text,  -- word — исходное слово
  s :: [Text]    -- s — подсказка (может быть несколько или могут отсутствовать).
                               }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type SpellResult = [SpellRevision]

