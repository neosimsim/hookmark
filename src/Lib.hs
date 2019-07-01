{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , writeBookmark
  , Criteria(..)
  , readBookmarkEntry
  , parseBookmarkEntry
  , renderBookmarkEntry
  , lookupBookmark
  , Bookmark
  , BookmarkEntry(..)
  , normalizePath
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
import           System.Posix.Files
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
  let path = joinPath [baseDir, normalizePath $ bookmarkName bm]
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

data Criteria =
  Criteria (Maybe FilePath) [Tag]

normalizePath :: FilePath -> FilePath
normalizePath = dropTrailingPathSeparator . snd . splitDrive

lookupBookmark :: FilePath -> Criteria -> IO [FilePath]
lookupBookmark baseDir (Criteria criteriaName []) = do
  createDirectoryIfMissing True baseDir
  withCurrentDirectory baseDir . listDirectories $
    normalizePath (fromMaybe mempty criteriaName)
lookupBookmark baseDir (Criteria criteriaName criteriaTags) = do
  names <- lookupBookmark baseDir (Criteria criteriaName [])
  marks <- zip names <$> mapM (readBookmarkEntry baseDir) names
  let x = filter (markIsTagged . snd) marks
  return $ fmap fst x
  where
    markIsTagged :: Either Text BookmarkEntry -> Bool
    markIsTagged (Left _)  = False
    markIsTagged (Right x) = criteriaTags `sublist` tags x

sublist :: (Traversable t, Eq a) => t a -> t a -> Bool
sublist xs ys = all (`elem` ys) xs

listDirectories :: FilePath -> IO [FilePath]
listDirectories "" = fmap (drop 2) <$> listDirectories "."
listDirectories path = do
  exists <- doesPathExist path
  if exists
    then do
      isDir <- isDirectory <$> getFileStatus path
      if isDir
        then do
          files <- listDirectory path
          x <- mapM listDirectories $ fmap (\x -> joinPath [path, x]) files
          return $ join x
        else return [path]
    else return []
