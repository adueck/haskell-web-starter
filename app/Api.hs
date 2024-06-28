{-# LANGUAGE OverloadedStrings #-}

module Api
  ( getBooks,
    addBook,
    removeBook,
    updateBook,
  )
where

import Control.Monad.IO.Class
import DB qualified
import Data.Aeson
import Database.PostgreSQL.Simple
import Network.HTTP.Types.Status
import Types
import Web.Scotty

updateBook :: Connection -> ActionM ()
updateBook conn = do
  (BookUpdate oldTitle b) :: BookUpdate <- jsonData
  res <- liftIO $ DB.updateBook conn oldTitle b
  case res of
    Left err -> do
      status status400
      json $ object ["error" .= err]
    Right _ -> do
      json $ object ["message" .= ("Book updated" :: String)]

getBooks :: Connection -> ActionM ()
getBooks conn = do
  books <- liftIO $ DB.getBooks conn
  json $ object ["books" .= books]

addBook :: Connection -> ActionM ()
addBook conn = do
  book <- jsonData
  res <- liftIO $ DB.addBook conn book
  case res of
    Left err -> do
      status status400
      json $ object ["error" .= err]
    Right _ -> do
      json $ object ["message" .= ("Book added" :: String)]

removeBook :: Connection -> ActionM ()
removeBook conn = do
  title <- pathParam "book"
  res <- liftIO $ DB.removeBook conn title
  case res of
    Left err -> do
      status status404
      json $ object ["error" .= err]
    Right _ -> do
      json $ object ["message" .= ("Book removed" :: String)]