{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , writeBookmark
  , readBookmarkEntry
  , parseBookmarkEntry
  , renderBookmarkEntry
  , Bookmark
  , BookmarkEntry(..)
  ) where

import           Control.Monad
import           Data.Either.Combinators
import           Data.NonEmptyText       as T
import           Data.Text               as T hiding (all, drop, empty, filter,
                                               span, zip)
import           Data.Text.IO            as T
import           Data.Tuple
import           Protolude
import           System.Directory
import           System.FilePath
import           Text.Megaparsec         as MP hiding (empty, many)
import           Text.Megaparsec.Char    as MP

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"

type Tag = NonEmptyText

type Bookmark = (FilePath, BookmarkEntry)

bookmarkName :: Bookmark -> FilePath
bookmarkName = fst

data BookmarkEntry =
  BookmarkEntry
    { url         :: Text
    , tags        :: [Tag]
    , description :: Text
    }
  deriving (Show, Eq)

parseBookmarkEntry :: Text -> Either Text BookmarkEntry
parseBookmarkEntry =
  mapLeft (pack . errorBundlePretty) . parse bookmarkEntryParser ""

type Parser = Parsec Void Text

bookmarkEntryParser :: Parser BookmarkEntry
bookmarkEntryParser =
  BookmarkEntry . pack <$> manyTill anySingle parseNewLineOrEnd <*>
  manyTill parseTag parseNewLineOrEnd <*>
  (pack <$> manyTill anySingle eof)

parseTag :: Parser Tag
parseTag = T.new <$> anySingle <*> (pack <$> manyTill anySingle newline)

parseNewLineOrEnd :: Parser ()
parseNewLineOrEnd = MP.try (void newline) <|> eof

renderBookmarkEntry :: BookmarkEntry -> Text
renderBookmarkEntry bm =
  T.concat
    [unlines ([url bm] ++ fmap toText (tags bm) ++ [mempty]), description bm]

writeBookmark :: FilePath -> Bookmark -> IO ()
writeBookmark baseDir bm = do
  let path = joinPath [baseDir, bookmarkName bm]
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path . renderBookmarkEntry $ snd bm

readBookmarkEntry :: FilePath -> FilePath -> IO (Either Text BookmarkEntry)
readBookmarkEntry baseDir bmName =
  withCurrentDirectory baseDir $ do
    exists <- doesFileExist bmName
    if exists
      then do
        parsed <- parseBookmarkEntry <$> readFile bmName
        case parsed of
          Right bm -> return $ Right bm
          Left s   -> fail $ unpack s
      else return $ Left "not found"
