{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hookmark.Command
  ( Command(..)
  , executeCommand
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString      as BS
import           Data.List            (sort)
import           Data.Maybe
import qualified Data.NonEmptyText    as T
import           Data.Path
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           Data.Version         (showVersion)
import           Distribution.Git
import           Hookmark.IO
import           Hookmark.Types
import           Paths_hookmark
import           System.Directory
import           System.Editor
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp       (withSystemTempDirectory)
import           System.Process.Typed

programVersion :: String
programVersion =
  case $(compileAbbrHash) of
    Nothing      -> showVersion version
    Just gitHash -> showVersion version ++ "-" ++ gitHash

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
      else return ""
  base <- fromMaybeBaseDir dir
  let bookmark = (n, BookmarkEntry u (fmap (fromJust . T.fromText) t) d)
  saveBookmark base bookmark
executeCommand (ShowBookmark dir t n') = do
  base <- fromMaybeBaseDir dir
  let n = normalizePath <$> n'
  let criteria =
        BookmarkCriteria
          { criteriaBookmarkName =
              n >>= \x ->
                if null x
                  then Nothing
                  else Just x
          , criteriaTags =
              if null t
                then Nothing
                else Just $ fmap (fromJust . T.fromText) t
          }
  lu <- filter (matchesCriteria criteria) <$> loadBookmarks base
  case lu of
    [] -> do
      T.hPutStrLn stderr "no bookmark found"
      exitWith $ ExitFailure 1
    [(_, entry)] -> BS.putStr . T.encodeUtf8 $ renderBookmarkEntry entry
    _ -> mapM_ putStrLn . sort $ fst <$> lu
executeCommand (EditBookmark base fileName') = do
  baseDir <- fromMaybeBaseDir base
  let fileName = normalizePath fileName'
  bookmark <- loadBookmark baseDir fileName
  bookmark' <-
    withSystemTempDirectory "hookmark" $ \tempBase ->
      withCurrentDirectory tempBase $ do
        saveBookmark "." bookmark
        editFile fileName
        loadBookmark "." fileName
  saveBookmark baseDir bookmark'
executeCommand (RemoveBookmark dir n') = do
  let n = normalizePath n'
  base <- fromMaybeBaseDir dir
  removeBookmark base n
executeCommand (MoveBookmark dir marks dest) = do
  base <- fromMaybeBaseDir dir
  destIsDir <-
    withCurrentDirectory base . doesDirectoryExist $ normalizePath dest
  if destIsDir
    then forM_ marks $ \mark -> moveBookmark base mark dest
    else case marks of
           [x] -> renameBookmark base x dest
           _ -> do
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
