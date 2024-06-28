{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api qualified
import DB qualified
import Database.PostgreSQL.Simple
import Handlers qualified as H
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Web.Scotty

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectDatabase = "postgres",
      connectHost = "localhost",
      connectPassword = "1234"
    }

-- TODO RUN DB SETUP SCRIPT
main :: IO ()
main = do
  conn <- connect localPG
  DB.setupDB conn
  routes conn

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  -- middleware for CORS and static files served from /static
  middleware simpleCors
  middleware $ staticPolicy (noDots >-> addBase "static")
  -- Client-facing HTTP Handlers
  --  (web app)
  get "/" (H.home conn "")
  post "/" (H.receiveAddBookForm conn)
  post "/:book" (H.receiveEditBookForm conn)
  get "/:book" (H.showEditBook conn)
  post "/delete/:book" (H.receiveDeleteReq conn)
  -- JSON Api routs
  --  (REST API)
  get "/api/books" (Api.getBooks conn)
  post "/api/books" (Api.addBook conn)
  -- put "/api/books" (Api.updateBook conn)
  delete "/api/:book" (Api.removeBook conn)
