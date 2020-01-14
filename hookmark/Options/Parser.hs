{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Options.Parser
  ( parseOptions
  ) where

import           Options.Applicative
import           Options.Applicative.Help  hiding (fullDesc)
import           Options.Applicative.Types
import           Options.Types             (Options (..), SubCommand (..))
import           System.Environment        (getArgs)
import           Text.RawString.QQ         (r)

programDescription :: String
programDescription = [r|Store, edit and search bookmarks.|]

parseOptions :: IO Options
parseOptions =
  eP $
  info
    (optionsParser <**> helper)
    (fullDesc <>
     progDesc programDescription <>
     header "hookmark — browser independent bookmarking")

optionsParser :: Parser Options
optionsParser =
  Version <$
  switch (long "version" <> short 'v' <> help "Show version" <> hidden) <|>
  Options <$> baseDirParser <*>
  hsubparser
    (command "add" (info addParser (progDesc "Add bookmark")) <>
     command "show" (info showParser (progDesc "Show or search bookmark")) <>
     command "edit" (info editParser (progDesc "Edit bookmark")) <>
     command "rm" (info removeParser (progDesc "Remove bookmark")) <>
     command "mv" (info moveParser (progDesc "Move bookmark")) <>
     command
       "git"
       (info
          gitParser
          (progDesc "Invoke git within hookmark directory" <>
           footerDoc
             (Just $
              text "Once git is setup, e.g. by" .$.
              indent 2 "hookmark git -- init" .$.
              "followed by" .$.
              indent 2 "hookmark git -- add ." .$.
              "and" .$.
              indent 2 "hookmark git -- commit -m 'initial commit'" .$.
              "hookmark will automatically track changes to bookmarks using git."))) <>
     command "version" (info (pure VersionBookmark) (progDesc "Show version")))

baseDirParser :: Parser (Maybe FilePath)
baseDirParser =
  optional
    (strOption
       (short 'b' <>
        metavar "BASEDIR" <>
        help
          "Base directory to store bookmarks in. Overwrites $HOOKMARKHOME. Defaults to $HOME/.hookmarks"))

addParser :: Parser SubCommand
addParser =
  AddBookmark <$>
  many
    (strOption
       (long "tag" <>
        short 't' <> help "Bookmark tag, can be used multiple times")) <*>
  switch (short 'd' <> help "Prompt for description") <*>
  strArgument
    (metavar "name" <>
     help "Name of the bookmark, can contain '/' to build hierarchies") <*>
  strArgument (metavar "url" <> help "URL of the bookmark")

showParser :: Parser SubCommand
showParser =
  ShowBookmark <$>
  many
    (strOption
       (long "tag" <>
        short 't' <> help "Bookmark tag, can be used multiple times")) <*>
  optional
    (strArgument
       (metavar "name" <>
        help "Name of the bookmark, can contain '/' to build hierarchies"))

editParser :: Parser SubCommand
editParser =
  EditBookmark <$>
  strArgument
    (metavar "name" <>
     help "Name of the bookmark to edit, can contain '/' to build hierarchies")

removeParser :: Parser SubCommand
removeParser =
  RemoveBookmark <$>
  strArgument (metavar "name" <> help "Name of the bookmark to delete")

moveParser :: Parser SubCommand
moveParser = applyInit <$> p <*> some (strArgument (metavar "source... dest"))
  where
    p :: Parser ([String] -> String -> SubCommand)
    p = pure MoveBookmark

gitParser :: Parser SubCommand
gitParser =
  ExecBookmark <$> pure "git" <*>
  ((:) <$> strArgument (metavar "GITCMD" <> help "git subcommand to invoke") <*>
   many
     (strArgument (metavar "GITARGS" <> help "Arguments to be passed to GITCMD")))

applyInit :: ([a] -> a -> b) -> [a] -> b
applyInit f xs =
  let i = init xs
      l = last xs
   in f i l

eP :: ParserInfo Options -> IO Options
eP pinfo = wrapper pinfo <$> getArgs >>= handleParseResult

wrapper :: ParserInfo Options -> [String] -> ParserResult Options
wrapper pinfo args = do
  cmd <- execParserPure defaultPrefs pinfo args
  case subCommand cmd of
    MoveBookmark {moveSources = sources} ->
      if null sources
        then Failure $
             parserFailure
               defaultPrefs
               pinfo
               (MissingError CmdCont (SomeParser moveParser))
               [ Context
                   "mv"
                   (info (moveParser <**> helper) (progDesc "Move bookmark"))
               ]
        else return cmd
    _ -> return cmd
