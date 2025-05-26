{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
  } deriving (Show, Eq, Generic)

instance ToJSON Book
instance FromJSON Book

-- Определение API
type BookAPI =
       "books" :> Get '[JSON] [Book]          -- Список книг
  :<|> "books" :> Capture "id" Int :> Get '[JSON] Book -- Книга по ID
  :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book -- Добавление книги
  -- :<|> "books" :> Capture "id" Int :> DeleteNoContent '[JSON] NoContent -- Удаление книги
  :<|> "books" :> Capture "id" Int :> DeleteNoContent

