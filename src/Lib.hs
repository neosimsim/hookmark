{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , writeBookmark
  , readBookmark
  , parseBookmark
  , renderBookmark
  , Bookmark(..)
  ) where

import           Data.Text        as T
import           Data.Text.IO     as T
import           Protolude
import           System.Directory
import           System.FilePath
import           Text.Read

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"

type Tag = Text

data Bookmark =
  Bookmark
    { url         :: Text
    , name        :: FilePath
    , tags        :: [Tag]
    , description :: Text
    }
  deriving (Show, Read, Eq)

parseBookmark :: Text -> Bookmark
parseBookmark = read . unpack

renderBookmark :: Bookmark -> Text
renderBookmark = pack . show

writeBookmark :: FilePath -> Bookmark -> IO ()
writeBookmark baseDir bm = do
  let path = joinPath [baseDir, name bm]
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path $ renderBookmark bm

readBookmark :: FilePath -> FilePath -> IO Bookmark
readBookmark baseDir bmName = do
  let path = joinPath [baseDir, bmName]
  parseBookmark <$> readFile path
