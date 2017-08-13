{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Docs where

import           Data.Char
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Text.HTML.Scalpel ((//), (@:), (@=))
import qualified Text.HTML.Scalpel as Scalpel

example =
  Url "https://hackage.haskell.org/package/haskell-src-1.0.2.0/docs/Language-Haskell-Parser.html"

newtype Url = Url { runUrl :: Text }
  deriving (Show, Eq, Ord)

stringUrl :: Url -> String
stringUrl = Text.unpack . runUrl

data DocPiece = DocPiece
  { docIdentifier :: Text
  , docPieceText  :: Text
  , docPieceLink  :: Url
  } deriving (Show, Eq, Ord)

format :: DocPiece -> Text
format DocPiece{..} = Text.concat
  [ docIdentifier
  , " — "
  , docPieceText
  , " "
  , runUrl docPieceLink
  ]

moduleDocPiece :: Url -> IO (Maybe DocPiece)
moduleDocPiece url = Scalpel.scrapeURL (stringUrl url) scraper
  where
    scraper :: Scalpel.Scraper String DocPiece
    scraper = do
      docIdentifier <- moduleName
      docPieceText  <- moduleAbstract
      let docPieceLink = url
      pure DocPiece{..}

    moduleName :: Scalpel.Scraper String Text
    moduleName = Text.pack <$> do
      Scalpel.text
        $ "div" @: ["id" @= "module-header"]
        // "p"  @: ["class" @= "caption"]

    moduleAbstract :: Scalpel.Scraper String Text
    moduleAbstract = head . Text.splitOn "." . Text.pack <$> do
      Scalpel.text
        $ "div" @: ["id" @= "content"]
        // "div"  @: ["id" @= "description"]
        // "div"  @: ["class" @= "doc"]

