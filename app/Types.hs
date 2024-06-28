{-# LANGUAGE OverloadedStrings #-}

module Types (Book (..)) where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    object,
    (.:),
  )

data Book = Book
  { title :: String,
    pages :: Int
  }
  deriving (Show, Eq)

instance FromJSON Book where
  parseJSON (Object o) =
    Book
      <$> o .: "title"
      <*> o .: "pages"
  parseJSON _ = fail "Expected an object for Product"

instance ToJSON Book where
  toJSON (Book title pages) =
    object
      [ "title" .= title,
        "pages" .= pages
      ]