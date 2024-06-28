{-# LANGUAGE OverloadedStrings #-}

module DB
  ( getBooks,
    queryBook,
    removeBook,
    addBook,
    updateBook,
    setupDB,
  )
where

import Control.Monad
import Database.PostgreSQL.Simple
import Types

toBook :: (String, Int) -> Book
toBook (title, pages) =
  Book
    { title = title,
      pages = pages
    }

getBooks :: Connection -> IO [Book]
getBooks conn = do
  res <- query_ conn "SELECT * FROM library" :: IO [(String, Int)]
  return $ fmap toBook res

queryBook :: Connection -> String -> IO (Maybe Book)
queryBook conn title = do
  x :: [(String, Int)] <-
    query
      conn
      "SELECT * FROM library WHERE title = ? "
      (Only title :: Only String)
  if null x
    then return Nothing
    else return $ Just (toBook $ head x)

removeBook :: Connection -> String -> IO (Either String Book)
removeBook conn title = do
  res <- queryBook conn title
  case res of
    Nothing -> return (Left "Book not found for deletion")
    Just b -> do
      r <-
        execute
          conn
          "DELETE FROM library WHERE title = ? "
          (Only title :: Only String)
      return $
        if r == 1
          then Right b
          else Left "Error deleting book"

addBook :: Connection -> Book -> IO (Either String Book)
addBook conn book = do
  res <- queryBook conn (title book)
  case res of
    Just _ -> return $ Left "Book already exists!"
    Nothing -> do
      r <- execute conn "INSERT INTO library VALUES (?, ?)" (title book, pages book)
      if r == 1
        then return $ Right book
        else return $ Left "Error adding book"

updateBook :: Connection -> String -> Book -> IO (Either String Book)
updateBook conn oldTitle updatedBook = do
  res <- queryBook conn oldTitle
  case res of
    Nothing -> return $ Left "Book not found!"
    Just _ -> do
      r <- execute conn "UPDATE library SET pages = ?, title = ? WHERE title = ?" (pages updatedBook, title updatedBook, oldTitle)
      if r == 1
        then return $ Right updatedBook
        else return $ Left "Error updating book"

setupDB :: Connection -> IO ()
setupDB conn = do
  a :: [(String, String)] <- query conn "SELECT schemaname, tablename FROM pg_tables WHERE tablename = ?" (Only "library" :: Only String)
  when (null a) $ do
    putStrLn "Creating library table..."
    _ <-
      execute
        conn
        "CREATE TABLE IF NOT EXISTS library ( title VARCHAR (100), pages INT ); INSERT INTO library VALUES ('Learn you a Haskell', 25), ('Haskell Design Patterns', 50)"
        ()
    return ()