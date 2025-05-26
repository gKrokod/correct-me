{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Тип данных для книги
data Book = Book
  { bookId    :: Int
  , bookTitle :: Text
  , bookAuthor :: Text
  } 
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Определение API
type BookAPI =
       "books" :> Get '[JSON] [Book]          -- Список книг
  :<|> "books" :> Capture "id" Int :> Get '[JSON] Book -- Книга по ID
  :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- Добавление книги
  :<|> "books" :> Capture "id" Int :> DeleteNoContent

