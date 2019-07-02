{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.List
import           Data.Maybe
import           Data.NonEmptyText
import           Data.Text                 as T hiding (null)
import           Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import           Data.Version              (showVersion)
import           Distribution.Git
import           Lib
import           Options.Applicative
import           Options.Applicative.Types
import           Paths_hookmark
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO                 (stderr)
import           System.Posix.Files
import           System.Process.Typed
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
  | EditBookmark
      { editBaseDir :: Maybe FilePath
      , editName    :: FilePath
      }
  | RemoveBookmark
      { removeBaseDir :: Maybe FilePath
      , removeName    :: FilePath
      }
  | MoveBookmark
      { moveBaseDir     :: Maybe FilePath
      , moveSources     :: [FilePath] -- TODO NonEmptyList
      , moveDestination :: FilePath
      }
  | ExecBookmark
      { execBaseDir    :: Maybe FilePath
      , execCmdSources :: String
      , execArgs       :: [String]
      }
  deriving (Show)

optionParser :: Parser Command
optionParser =
  Version <$
  switch (long "version" <> short 'v' <> help "Show version" <> hidden) <|>
  hsubparser
    (command "add" (info addParser (progDesc "Add bookmark")) <>
     command "show" (info showParser (progDesc "Show or search bookmark")) <>
     command "edit" (info editParser (progDesc "Edit bookmark")) <>
     command "rm" (info removeParser (progDesc "Remove bookmark")) <>
     command "mv" (info moveParser (progDesc "Move bookmark")) <>
     command
       "git"
       (info gitParser (progDesc "Invoke git within hookmark directory")) <>
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

editParser :: Parser Command
editParser =
  EditBookmark <$>
  optional
    (strOption
       (short 'b' <>
        help
          "Base directory to lookup bookmarks in. Default $HOOKMARKHOME or $HOME/.hookmarkhome if unset")) <*>
  strArgument
    (metavar "name" <>
     help "Name of the bookmark to edit, can contain '/' to build hierarchies")

removeParser :: Parser Command
removeParser =
  RemoveBookmark <$>
  optional
    (strOption
       (short 'b' <>
        help
          "Base directory to lookup bookmarks in. Default $HOOKMARKHOME or $HOME/.hookmarkhome if unset")) <*>
  strArgument (metavar "name" <> help "Name of the bookmark to delete")

moveParser :: Parser Command
moveParser = applyInit <$> p <*> some (strArgument (metavar "source... dest"))
  where
    p :: Parser ([String] -> String -> Command)
    p =
      MoveBookmark <$>
      optional
        (strOption
           (short 'b' <>
            help
              "Base directory to lookup bookmarks in. Default $HOOKMARKHOME or $HOME/.hookmarkhome if unset"))

gitParser :: Parser Command
gitParser =
  ExecBookmark <$>
  optional
    (strOption
       (short 'b' <>
        help
          "Base directory to lookup bookmarks in. Default $HOOKMARKHOME or $HOME/.hookmarkhome if unset")) <*>
  pure "git" <*>
  ((:) <$> strArgument (metavar "GITCMD" <> help "git subcommand to invoke") <*>
   many
     (strArgument (metavar "GITARGS" <> help "Arguments to be passed to GITCMD")))

applyInit :: ([a] -> a -> b) -> [a] -> b
applyInit f list =
  let i = Data.List.init list
      l = Data.List.last list
   in f i l

eP :: ParserInfo Command -> IO Command
eP pinfo = wrapper pinfo <$> getArgs >>= handleParseResult

wrapper :: ParserInfo Command -> [String] -> ParserResult Command
wrapper pinfo args = do
  cmd <- execParserPure defaultPrefs pinfo args
  case cmd of
    MoveBookmark {moveSources = sources} ->
      if Data.List.null sources
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

main :: IO ()
main = do
  opt <-
    eP $
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
      git <- isGit base
      when git (commitAll base $ "add " ++ n)
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
    EditBookmark dir n -> do
      base <- fromMaybeBaseDir dir
      lu <- lookupBookmark base (Criteria (Just n) [])
      case lu of
        [] -> do
          T.hPutStrLn stderr "not found"
          exitWith $ ExitFailure 1
        [x] -> do
          editor <- fromMaybe "vi" <$> lookupEnv "EDITOR"
          code <- runProcess $ proc editor [base </> x]
          case code of
            ExitFailure c -> do
              T.hPutStrLn stderr $ "editor failed: " `mappend` pack (show c)
              exitWith $ ExitFailure 1
            ExitSuccess -> do
              git <- isGit base
              when git (commitAll base $ "edit " ++ x)
        _ -> do
          T.hPutStrLn stderr "name is not unique"
          exitWith $ ExitFailure 1
    RemoveBookmark dir n -> do
      base <- fromMaybeBaseDir dir
      lu <- lookupBookmark base (Criteria (Just n) [])
      case lu of
        [] -> do
          T.hPutStrLn stderr "not found"
          exitWith $ ExitFailure 1
        [x] -> do
          removeFile $ base </> x
          withCurrentDirectory base . cleanDirectory $ takeDirectory x
          git <- isGit base
          when git (commitAll base $ "remove " ++ x)
        _ -> do
          T.hPutStrLn stderr "name is not unique"
          exitWith $ ExitFailure 1
    (MoveBookmark dir marks dest) -> do
      base <- fromMaybeBaseDir dir
      case marks of
        [x] -> do
          moveBookmark base x dest
          git <- isGit base
          when git (commitAll base $ "move " ++ x ++ " to " ++ dest)
        _ -> do
          destExists <- fileExist $ base </> dest
          destIsDir <-
            if destExists
              then isDirectory <$> getFileStatus (base </> dest)
              else return False
          if destIsDir
            then do
              mapM_ (\x -> moveBookmark base x dest) marks
              git <- isGit base
              when git (commitAll base $ "move to " ++ dest)
            else do
              T.hPutStrLn stderr $ pack dest `mappend` " is not a directory"
              exitWith $ ExitFailure 1
    (ExecBookmark dir cmd args) -> do
      base <- fromMaybeBaseDir dir
      withCurrentDirectory base $ do
        code <- runProcess $ proc cmd args
        case code of
          ExitFailure c -> do
            T.hPutStrLn stderr . pack $
              Data.List.concat [cmd, " failed (", show c, ")"]
            exitWith $ ExitFailure 1
          ExitSuccess -> return ()

commitAll :: FilePath -> String -> IO ()
commitAll base msg =
  withCurrentDirectory base $ do
    addCode <- runProcess $ proc "git" ["add", "."]
    case addCode of
      ExitFailure cadd -> do
        T.hPutStrLn stderr . pack $
          Data.List.concat ["auto add failed (", show cadd, ")"]
        exitWith $ ExitFailure 1
      ExitSuccess -> do
        commitCode <- runProcess $ proc "git" ["commit", "-m", msg]
        case commitCode of
          ExitFailure ccommit -> do
            T.hPutStrLn stderr . pack $
              Data.List.concat ["auto commit failed (", show ccommit, ")"]
            exitWith $ ExitFailure 1
          ExitSuccess -> return ()

isGit :: FilePath -> IO Bool
isGit base = do
  let gitDir = base </> ".git"
  destExists <- fileExist gitDir
  if destExists
    then isDirectory <$> getFileStatus gitDir
    else return False

moveBookmark :: FilePath -> FilePath -> FilePath -> IO ()
moveBookmark base f "/" = moveBookmark base f "."
moveBookmark base f d = do
  let dest = normalizePath d
  let file = normalizePath f
  withCurrentDirectory base $ do
    exists <- fileExist file
    if not exists
      then T.hPutStrLn stderr $ "not found " `mappend` pack file
      else do
        destExists <- fileExist dest
        isDir <-
          if destExists
            then isDirectory <$> getFileStatus dest
            else return False
        let destName =
              if isDir
                then dest </> takeFileName file
                else dest
        createDirectoryIfMissing True $ takeDirectory destName
        renamePath file destName
        cleanDirectory $ takeDirectory file

cleanDirectory :: FilePath -> IO ()
cleanDirectory "." = return ()
cleanDirectory path = do
  isDir <- isDirectory <$> getFileStatus path
  when isDir $ do
    isEmpty <- null <$> listDirectory path
    when isEmpty $ removeDirectory path >> cleanDirectory (takeDirectory path)

fromMaybeBaseDir :: Maybe FilePath -> IO FilePath
fromMaybeBaseDir dir = do
  baseFromEnv <- lookupEnv "HOOKMARKHOME"
  home <- getHomeDirectory
  return (fromMaybe (joinPath [home, ".hookmarks"]) $ dir <|> baseFromEnv)
