{-# LANGUAGE OverloadedStrings #-}

module Api
  ( getBooks,
    addBook,
    removeBook,
  )
where

import Control.Monad.IO.Class
import DB qualified
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Types
import Web.Scotty

-- updateBook :: Connection -> ActionM ()
-- updateBook conn = do
--   (Book title pages) <- jsonData
--   books <- queryBooks conn title
--   if null books
--     then json $ object ["error" .= ("Book not found!" :: String)]
--     else do
--       _ <- liftIO $ execute conn "UPDATE library SET pages = ? WHERE title = ?" (pages, title)
--       json $ object ["message" .= ("Updated book" :: String)]

-- getPages :: () -> ActionM (Maybe Int)
-- getPages _ = do
--   pagesRaw <- pathParam "pages"
--   let p = readMaybe pagesRaw :: Maybe Int
--   return p

getBooks :: Connection -> ActionM ()
getBooks conn = do
  books <- liftIO $ DB.getBooks conn
  json $ object ["books" .= books]

addBook :: Connection -> ActionM ()
addBook conn = do
  (Book title pages) <- jsonData
  res <- liftIO $ DB.queryBook conn title
  if not (null res)
    then do
      status status409
      json $ object ["error" .= ("Book already exists" :: String)]
    else do
      _ <- liftIO $ execute conn "INSERT INTO library VALUES (?, ?)" (title, pages)
      json $ object ["message" .= ("Book added" :: String)]

removeBook :: Connection -> ActionM ()
removeBook conn = do
  title <- pathParam "title"
  res <- liftIO $ DB.queryBook conn title
  if null res
    then do
      status status404
      json $ object ["error" .= ("Book not found for removal" :: String)]
    else do
      _ <-
        liftIO $
          execute
            conn
            "DELETE FROM library WHERE title = ? "
            (Only title :: Only String)
      json $ object ["message" .= ("Book removed" :: String)]