{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where

import           Control.Applicative
import qualified Data.ByteString     as BS
import           Data.List
import           Data.Maybe
import           Data.NonEmptyText
import           Data.Text           as T
import           Data.Text.Encoding  as T
import qualified Data.Text.IO        as T
import           Data.Version        (showVersion)
import           Distribution.Git
import           Lib
import           Options.Applicative
import           Paths_hookmark
import           System.Directory
import           System.Environment  (lookupEnv)
import           System.Exit
import           System.FilePath
import           System.IO           (stderr)
import           Text.RawString.QQ

programDescription :: String
programDescription = [r|Store, edit and search bookmarks.|]

programVersion :: String
programVersion = showVersion version ++ "-" ++ $(compileAbbrHash)

data Command
  = Version
  | AddBookmark
      { addBaseDir     :: Maybe FilePath
      , addTags        :: [Text]
      , addDescription :: Bool
      , addName        :: FilePath
      , addUrl         :: Text
      }
  | ShowBookmark
      { showBaseDir :: Maybe FilePath
      , showTags    :: [Text]
      , showName    :: Maybe FilePath
      }
  deriving (Show)

optionParser :: Parser Command
optionParser =
  Version <$
  switch (long "version" <> short 'v' <> help "Show version" <> hidden) <|>
  hsubparser
    (command "add" (info addParser (progDesc "Add bookmark")) <>
     command "show" (info showParser (progDesc "Show or search bookmark")) <>
     command "version" (info (pure Version) (progDesc "Show version")))

addParser :: Parser Command
addParser =
  AddBookmark <$>
  optional
    (strOption
       (short 'b' <>
        help
          "Base directory to store bookmarks in. Default $HOOKMARKHOME or $HOME/.hookmarkhome if unset")) <*>
  many
    (strOption
       (long "tag" <> short 't' <>
        help "Bookmark tag, can be used multiple times")) <*>
  switch (short 'd' <> help "Prompt for description") <*>
  strArgument
    (metavar "name" <>
     help "Name of the bookmark, can contain '/' to build hierarchies") <*>
  strArgument (metavar "url" <> help "URL of the bookmark")

showParser :: Parser Command
showParser =
  ShowBookmark <$>
  optional
    (strOption
       (short 'b' <>
        help
          "Base directory to lookup bookmarks in. Default $HOOKMARKHOME or $HOME/.hookmarkhome if unset")) <*>
  many
    (strOption
       (long "tag" <> short 't' <>
        help "Bookmark tag, can be used multiple times")) <*>
  optional
    (strArgument
       (metavar "name" <>
        help "Name of the bookmark, can contain '/' to build hierarchies"))

main :: IO ()
main = do
  opt <-
    execParser $
    info
      (optionParser <**> helper)
      (fullDesc <> progDesc programDescription <>
       header "hookmark — browser independent bookmarking")
  case opt of
    Version -> putStrLn programVersion
    AddBookmark dir t promptDesc n u -> do
      d <-
        if promptDesc
          then T.getContents
          else return mempty
      base <- fromMaybeBaseDir dir
      writeBookmark base (n, BookmarkEntry u (fmap (fromJust . fromText) t) d)
    ShowBookmark dir t n -> do
      base <- fromMaybeBaseDir dir
      lu <- lookupBookmark base (Criteria n (fmap (fromJust . fromText) t))
      case lu of
        [] -> do
          T.hPutStrLn stderr "not found"
          exitWith $ ExitFailure 1
        [x] -> do
          bs <- readBookmarkEntry base x
          case bs of
            Right bss -> BS.putStr . T.encodeUtf8 $ renderBookmarkEntry bss
            Left err -> do
              BS.putStr . T.encodeUtf8 $ err
              exitWith $ ExitFailure 1
        xs -> mapM_ putStrLn $ sort xs

fromMaybeBaseDir :: Maybe FilePath -> IO FilePath
fromMaybeBaseDir dir = do
  baseFromEnv <- lookupEnv "HOOKMARKHOME"
  home <- getHomeDirectory
  return (fromMaybe (joinPath [home, ".hookmarks"]) $ dir <|> baseFromEnv)
