{-# LANGUAGE OverloadedStrings #-}

module Handlers
  ( indexBooks,
    showBook,
    updateBook,
    destroyBook,
    createBook,
  )
where

import Control.Monad.IO.Class
import DB qualified
import Data.Text (unpack)
import Database.PostgreSQL.Simple
import Network.URI.Encode
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Read
import Types
import Web.Scotty

indexBooks :: Connection -> String -> ActionM ()
indexBooks conn msg = do
  books <- liftIO $ DB.getBooks conn
  indexPage msg books

showBook :: Connection -> ActionM ()
showBook conn = do
  title :: String <- pathParam "book"
  book <- liftIO $ DB.queryBook conn title
  case book of
    Nothing -> do
      redirect "/"
    Just b -> do
      bookPage b

updateBook :: Connection -> ActionM ()
updateBook conn = do
  ps <- formParams
  oldTitle <- pathParam "book"
  let book = bookFromParams ps
  case book of
    Nothing -> redirect "/"
    Just b -> do
      res <- liftIO $ DB.updateBook conn oldTitle b
      case res of
        Left _ -> redirect "/"
        Right _ -> redirect "/"

destroyBook :: Connection -> ActionM ()
destroyBook conn = do
  title <- pathParam "book"
  _ <- liftIO $ DB.removeBook conn title
  redirect "/"

createBook :: Connection -> ActionM ()
createBook conn = do
  ps <- formParams
  let book = bookFromParams ps
  case book of
    Nothing -> indexBooks conn "invalid submission"
    Just b -> do
      res <- liftIO $ DB.addBook conn b
      case res of
        Left err -> indexBooks conn err
        Right _ -> indexBooks conn ""

-- Views

indexPage :: String -> [Book] -> ActionM ()
indexPage msg books = html $
  renderHtml $
    template "My Library" $ do
      booksTable books
      H.h4 "Add book"
      bookForm "" 0 "/" "Add Book"
      H.p $ H.toHtml msg

bookPage :: Book -> ActionM ()
bookPage book = html $
  renderHtml $
    template "My Library / Edit Book" $ do
      bookForm (title book) (pages book) ("/" ++ encode (title book)) "Edit Book"

-- HTML Components

template :: String -> H.Html -> H.Html
template title children =
  H.html $ do
    H.head $ do
      H.title (H.toHtml title)
      H.link H.! A.href "/css/bootstrap.min.css" H.! A.rel "stylesheet"
    H.body $ do
      H.div H.! A.class_ "container py-4" $ do
        H.h1 (H.toHtml title) H.! A.class_ "mb-4"
        children

bookForm :: String -> Int -> String -> String -> H.Html
bookForm titleVal pagesVal actionPath buttonText =
  H.form H.! A.class_ "row g-3" H.! A.method "post" H.! A.action (stringValue actionPath) $ do
    H.div H.! A.class_ "col-md-6" $ do
      H.label "Title:" H.! A.for "title"
      H.input H.! A.class_ "form-control" H.! A.type_ "text" H.! A.name "title" H.! A.value (stringValue titleVal)
    H.div H.! A.class_ "col-md-6" $ do
      H.label "Pages read:" H.! A.for "pages"
      H.input
        H.! A.class_ "form-control"
        H.! A.type_ "number"
        H.! A.name "pages"
        H.! A.value (if pagesVal > 0 then stringValue $ show pagesVal else "")
    H.div H.! A.class_ "col-12" $ do
      H.button (H.toHtml buttonText) H.! A.type_ "submit" H.! A.class_ "btn btn-primary"

booksTable :: [Book] -> H.Html
booksTable books = H.table H.! A.class_ "table" $ do
  H.tr $ do
    H.th "Title" H.! A.scope "col"
    H.th "Pages Read" H.! A.scope "col"
    H.th "" H.! A.scope "col"
    H.th "" H.! A.scope "col"
  mapM_
    ( \(Book title pages) ->
        H.tr $ do
          H.td $ H.toHtml title
          H.td $ H.toHtml pages
          H.td $ H.a "edit" H.! A.class_ "btn btn-light btn-sm" H.! A.href (stringValue ("/" ++ encode title))
          H.td $ do
            H.form H.! A.method "POST" H.! A.action (stringValue ("/delete/" ++ encode title)) $ do
              H.button "Delete" H.! A.type_ "submit" H.! A.class_ "btn btn-danger btn-sm"
    )
    books

-- Helper Functions

bookFromParams :: [Param] -> Maybe Book
bookFromParams p = do
  title <- lookup "title" p
  let titleStr = unpack title
  pagesRaw <- lookup "pages" p
  pages :: Int <- readMaybe $ unpack pagesRaw
  if null titleStr
    then Nothing
    else
      Just
        Book
          { title = titleStr,
            pages = pages
          }