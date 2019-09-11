{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hookmark.Command
  ( Command(..)
  , executeCommand
  ) where

import           Control.Monad
import qualified Data.ByteString      as BS
import           Data.Maybe
import qualified Data.NonEmptyText    as T
import           Data.Path
import           Data.String          (String)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           Data.Version         (showVersion)
import           Distribution.Git
import           Hookmark.Parser
import           Hookmark.Types
import           Paths_hookmark
import           Protolude
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Posix.Files
import           System.Process.Typed

data Criteria =
  Criteria (Maybe FilePath) [Tag]

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

executeCommand :: Command -> IO ()
executeCommand Version = putStrLn programVersion
executeCommand (AddBookmark dir t promptDesc n u) = do
  d <-
    if promptDesc
      then T.getContents
      else return mempty
  base <- fromMaybeBaseDir dir
  writeBookmark base (n, BookmarkEntry u (fmap (fromJust . T.fromText) t) d)
  git <- isGit base
  when git (commitAll base $ "add " ++ n)
executeCommand (ShowBookmark dir t n) = do
  base <- fromMaybeBaseDir dir
  lu <- lookupBookmark base (Criteria n (fmap (fromJust . T.fromText) t))
  case lu of
    [] -> do
      T.hPutStrLn stderr "no bookmark found"
      exitWith $ ExitFailure 1
    [x] -> do
      bs <- readBookmarkEntry base x
      case bs of
        Right bss -> BS.putStr . T.encodeUtf8 $ renderBookmarkEntry bss
        Left err -> do
          BS.putStr . T.encodeUtf8 $ err
          exitWith $ ExitFailure 1
    xs -> mapM_ putStrLn $ sort xs
executeCommand (EditBookmark dir n) = do
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
          T.hPutStrLn stderr $ "editor failed: " `mappend` T.pack (show c)
          exitWith $ ExitFailure 1
        ExitSuccess -> do
          git <- isGit base
          when git (commitAll base $ "edit " ++ x)
    _ -> do
      T.hPutStrLn stderr "name is not unique"
      exitWith $ ExitFailure 1
executeCommand (RemoveBookmark dir n) = do
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
executeCommand (MoveBookmark dir marks dest) = do
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
          T.hPutStrLn stderr $ T.pack dest `mappend` " is not a directory"
          exitWith $ ExitFailure 1
executeCommand (ExecBookmark dir cmd args) = do
  base <- fromMaybeBaseDir dir
  withCurrentDirectory base $ do
    code <- runProcess $ proc cmd args
    case code of
      ExitFailure c -> do
        T.hPutStrLn stderr . T.pack $ concat [cmd, " failed (", show c, ")"]
        exitWith $ ExitFailure 1
      ExitSuccess -> return ()

fromMaybeBaseDir :: Maybe FilePath -> IO FilePath
fromMaybeBaseDir dir = do
  baseFromEnv <- lookupEnv "HOOKMARKHOME"
  home <- getHomeDirectory
  return (fromMaybe (joinPath [home, ".hookmarks"]) $ dir <|> baseFromEnv)

commitAll :: FilePath -> String -> IO ()
commitAll base msg =
  withCurrentDirectory base $ do
    addCode <- runProcess $ proc "git" ["add", "."]
    case addCode of
      ExitFailure cadd -> do
        T.hPutStrLn stderr . T.pack $
          concat ["auto add failed (", show cadd, ")"]
        exitWith $ ExitFailure 1
      ExitSuccess -> do
        commitCode <- runProcess $ proc "git" ["commit", "-m", msg]
        case commitCode of
          ExitFailure ccommit -> do
            T.hPutStrLn stderr . T.pack $
              concat ["auto commit failed (", show ccommit, ")"]
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
      then do
        T.hPutStrLn stderr $ "not found " `mappend` T.pack file
        exitWith $ ExitFailure 1
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

lookupBookmark :: FilePath -> Criteria -> IO [FilePath]
lookupBookmark baseDir (Criteria criteriaName []) = do
  createDirectoryIfMissing True baseDir
  withCurrentDirectory baseDir $
    filter ((/= ".git") . Protolude.take 4) <$>
    listDirectories (normalizePath (fromMaybe mempty criteriaName))
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

readBookmarkEntry :: FilePath -> FilePath -> IO (Either Text BookmarkEntry)
readBookmarkEntry baseDir bmName =
  withCurrentDirectory baseDir $ do
    exists <- doesFileExist bmName
    if exists
      then do
        parsed <- parseBookmarkEntry <$> readFile bmName
        case parsed of
          Right bm -> return $ Right bm
          Left s   -> fail $ T.unpack s
      else return $ Left "not found"

writeBookmark :: FilePath -> Bookmark -> IO ()
writeBookmark baseDir bm = do
  let path = joinPath [baseDir, normalizePath $ bookmarkName bm]
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path . renderBookmarkEntry $ snd bm
