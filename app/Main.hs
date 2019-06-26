{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           Control.Applicative
import           Data.Maybe
import           Data.NonEmptyText
import           Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Version        (showVersion)
import           Distribution.Git
import           Lib
import           Options.Applicative
import           Paths_hookmark
import           System.Environment  (lookupEnv)
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
  deriving (Show)

optionParser :: Parser Command
optionParser =
  Version <$
  switch (long "version" <> short 'v' <> help "Show version" <> hidden) <|>
  hsubparser
    (command "add" (info addParser (progDesc "Add bookmark")) <>
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
      baseFromEnv <- lookupEnv "HOOKMARKHOME"
      writeBookmark
        (fromMaybe "$HOME/.hookmark" $ dir <|> baseFromEnv)
        (n, BookmarkEntry u (fmap (fromJust . fromText) t) d)
