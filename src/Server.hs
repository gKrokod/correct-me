{-# LANGUAGE FlexibleContexts #-}
module Server where

import Servant
import Network.Wai.Handler.Warp (run)
import Api
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- Начальные данные
initialBooks :: [Book]
initialBooks = [ Book 1 "Haskell Programming" "John Doe"
               , Book 2 "Learn You a Haskell" "Miran Lipovača"
               ]

-- Обработчики маршрутов
server :: IORef [Book] -> Server BookAPI
server booksRef =
       getBooks
  :<|> getBookById
  :<|> addBook
  :<|> deleteBook
  where
    -- Возвращает список книг
    getBooks = liftIO $ readIORef booksRef

    -- Возвращает книгу по ID
    getBookById bid = do
      books <- liftIO $ readIORef booksRef
      case filter ((== bid) . bookId) books of
        [book] -> return book
        _      -> throwError err404

    -- Добавляет новую книгу
    addBook newBook = liftIO $ do
      modifyIORef booksRef (newBook :)
      return newBook

    -- Удаляет книгу по ID
    deleteBook bid = liftIO $ do
      modifyIORef booksRef (filter ((/= bid) . bookId))
      return NoContent

-- Запуск сервера
startApp :: IO ()
startApp = do
  booksRef <- newIORef initialBooks
  putStrLn "Server running on http://localhost:8080"
  run 8080 $ serve (Proxy :: Proxy BookAPI) (server booksRef)
