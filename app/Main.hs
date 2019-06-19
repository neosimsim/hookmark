{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import           Control.Applicative
import           Data.Version        (showVersion)
import           Distribution.Git
import           Options.Applicative
import           Paths_hookmark
import           Text.RawString.QQ

programDescription :: String
programDescription = [r|Store, edit and search bookmarks.|]

programVersion :: String
programVersion = showVersion version ++ "-" ++ $(compileAbbrHash)

data Command =
  Version

optionParser :: Parser Command
optionParser =
  Version <$
  switch (long "version" <> short 'v' <> help "Show version" <> hidden) <|>
  subparser (command "version" (info (pure Version) (progDesc "Show version")))

main :: IO ()
main = do
  opts <-
    execParser $
    info
      (optionParser <**> helper)
      (header "hookmark — browser independent bookmarking" <>
       progDesc programDescription)
  case opts of
    Version -> putStrLn programVersion
