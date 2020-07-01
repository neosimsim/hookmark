{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  )
where

import           Control.Applicative            ( (<|>) )

import           Control.Exception              ( catch )
import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
                                                ( putStr )
import           Data.List                      ( sort )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import qualified Data.NonEmptyText             as NonEmptyText
                                                ( fromText )
import           Data.Path                      ( toRelativeFilePath )
import qualified Data.Text                     as Text
                                                ( pack )
import qualified Data.Text.Encoding            as Text
                                                ( encodeUtf8 )
import qualified Data.Text.IO                  as Text
                                                ( getContents
                                                , hPutStrLn
                                                )
import           Data.Version                   ( showVersion )
import           Distribution.Git               ( compileAbbrHash )
import           Hookmark.IO                    ( BookmarkCriteria(..)
                                                , HookmarkException(..)
                                                , loadBookmark
                                                , loadBookmarks
                                                , matchesCriteria
                                                , moveBookmark
                                                , removeBookmark
                                                , renameBookmark
                                                , saveBookmark
                                                )
import           Hookmark.Types                 ( BookmarkEntry(..)
                                                , renderBookmarkEntry
                                                )
import           Options                        ( Options(..)
                                                , parseOptions
                                                )
import           Paths_hookmark                 ( version )
import           System.Directory               ( doesDirectoryExist
                                                , getHomeDirectory
                                                , withCurrentDirectory
                                                )
import           System.Editor                  ( editFile )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.FilePath                ( joinPath )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.IO.Temp                 ( withSystemTempDirectory )
import           System.Process.Typed           ( proc
                                                , runProcess
                                                )

main :: IO ()
main = do
  options <- parseOptions
  catch (runWithOptions options) handleExceptions
 where
  handleExceptions :: HookmarkException -> IO a
  handleExceptions HookmarkException { fromHookmarkException = msg } = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1

programVersion :: String
programVersion = case $(compileAbbrHash) of
  Nothing      -> showVersion version
  Just gitHash -> showVersion version ++ "-" ++ gitHash

runWithOptions :: Options -> IO ()
runWithOptions Version                            = putStrLn programVersion
runWithOptions (AddBookmark dir t promptDesc n u) = do
  d    <- if promptDesc then Text.getContents else return ""
  base <- fromMaybeBaseDir dir
  let bookmark =
        (n, BookmarkEntry u (fmap (fromJust . NonEmptyText.fromText) t) d)
  saveBookmark base bookmark
runWithOptions (ShowBookmark dir t n') = do
  base <- fromMaybeBaseDir dir
  let n = toRelativeFilePath <$> n'
  let
    criteria = BookmarkCriteria
      { criteriaBookmarkName = n
        >>= \x -> if null x || x == "." then Nothing else Just x
      , criteriaTags         = if null t
                                 then Nothing
                                 else Just $ fmap (fromJust . NonEmptyText.fromText) t
      }
  lu <- filter (matchesCriteria criteria) <$> loadBookmarks base
  case lu of
    [] -> do
      Text.hPutStrLn stderr "no bookmark found"
      exitWith $ ExitFailure 1
    [(_, entry)] -> BS.putStr . Text.encodeUtf8 $ renderBookmarkEntry entry
    _            -> mapM_ putStrLn . sort $ fst <$> lu
runWithOptions (EditBookmark base fileName') = do
  baseDir <- fromMaybeBaseDir base
  let fileName = toRelativeFilePath fileName'
  bookmark  <- loadBookmark baseDir fileName
  bookmark' <- withSystemTempDirectory "hookmark" $ \tempBase ->
    withCurrentDirectory tempBase $ do
      saveBookmark "." bookmark
      editFile fileName
      loadBookmark "." fileName
  saveBookmark baseDir bookmark'
runWithOptions (RemoveBookmark dir n') = do
  let n = toRelativeFilePath n'
  base <- fromMaybeBaseDir dir
  removeBookmark base n
runWithOptions (MoveBookmark dir marks dest) = do
  base      <- fromMaybeBaseDir dir
  destIsDir <-
    withCurrentDirectory base . doesDirectoryExist $ toRelativeFilePath dest
  if destIsDir
    then forM_ marks $ \mark -> moveBookmark base mark dest
    else case marks of
      [x] -> renameBookmark base x dest
      _   -> do
        Text.hPutStrLn stderr $ Text.pack dest `mappend` " is not a directory"
        exitWith $ ExitFailure 1
runWithOptions (ExecBookmark dir cmd args) = do
  base <- fromMaybeBaseDir dir
  withCurrentDirectory base $ do
    code <- runProcess $ proc cmd args
    case code of
      ExitFailure c -> do
        Text.hPutStrLn stderr . Text.pack $ concat
          [cmd, " failed (", show c, ")"]
        exitWith $ ExitFailure 1
      ExitSuccess -> return ()

fromMaybeBaseDir :: Maybe FilePath -> IO FilePath
fromMaybeBaseDir dir = do
  baseFromEnv <- lookupEnv "HOOKMARKHOME"
  home        <- getHomeDirectory
  return (fromMaybe (joinPath [home, ".hookmarks"]) $ dir <|> baseFromEnv)
