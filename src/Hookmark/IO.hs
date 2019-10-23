{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Hookmark.IO
  ( BookmarkCriteria(..)
  , saveBookmark
  , removeBookmark
  , loadBookmark
  , matchesCriteria
  , moveBookmark
  , renameBookmark
  , loadBookmarks
  , HookmarkException(..)
  ) where

import           Control.Exception      (Exception (..), throw)
import           Control.Monad.Extra    (ifM, whenM)
import           Data.List              (isPrefixOf)
import           Data.Path              (normalizePath)
import qualified Data.Text              as Text (unpack)
import qualified Data.Text.IO           as Text (readFile, writeFile)
import           Git                    (commitAll, isGitRepo)
import           Hookmark.Parser        (parseBookmarkEntry)
import           Hookmark.Types         (Bookmark, BookmarkEntry (..),
                                         BookmarkName, Tag, bookmarkName,
                                         renderBookmarkEntry)
import           System.Directory       (createDirectoryIfMissing,
                                         doesDirectoryExist, doesPathExist,
                                         removeFile, renamePath,
                                         withCurrentDirectory)
import           System.Directory.Extra (cleanDirectory, listDirectories)
import           System.FilePath        (takeDirectory, takeFileName, (</>))

-- | Save the given bookmark. Existing bookmarks will be overwritten
saveBookmark :: FilePath -> Bookmark -> IO ()
saveBookmark baseDir bookmark = do
  createDirectoryIfMissing True baseDir
  withCurrentDirectory baseDir $ do
    let path = normalizePath $ bookmarkName bookmark
    bookmarksExists <- doesPathExist path
    createDirectoryIfMissing True $ takeDirectory path
    Text.writeFile path . renderBookmarkEntry $ snd bookmark
    whenM
      (isGitRepo ".git")
      (commitAll $
       (if bookmarksExists
          then "edit "
          else "add ") ++
       bookmarkName bookmark)

removeBookmark :: FilePath -> BookmarkName -> IO ()
removeBookmark baseDir name =
  withCurrentDirectory baseDir $ do
    removeFile name
    cleanDirectory $ takeDirectory name
    whenM (isGitRepo ".git") (commitAll $ "remove " ++ name)

data BookmarkCriteria =
  BookmarkCriteria
    { criteriaBookmarkName :: Maybe FilePath -- ^ name of a bookmark or subtree
    , criteriaTags         :: Maybe [Tag]
    }
  deriving (Show)

matchesCriteria :: BookmarkCriteria -> Bookmark -> Bool
matchesCriteria BookmarkCriteria {..} (name, BookmarkEntry {..}) =
  nameMatches && tagsMatches
  where
    sublist :: (Traversable t, Eq a) => t a -> t a -> Bool
    sublist xs ys = all (`elem` ys) xs
    nameMatches :: Bool
    nameMatches =
      case criteriaBookmarkName of
        Nothing -> True
        Just x  -> x `isPrefixOf` name
    tagsMatches :: Bool
    tagsMatches =
      case criteriaTags of
        Nothing -> True
        Just x  -> x `sublist` tags

-- | Rename a bookmark. Overwrites exist destination. The destination
-- path will be created if not present.
renameBookmark :: FilePath -> BookmarkName -> BookmarkName -> IO ()
renameBookmark baseDir oldName' newName' = do
  let oldName = normalizePath oldName'
  let newName = normalizePath newName'
  withCurrentDirectory baseDir $
    -- TODO error message if destination is an existing directory
   do
    createParent newName
    renamePath oldName newName
    cleanDirectory $ takeDirectory oldName
    whenM
      (isGitRepo ".git")
      (commitAll $ "move " ++ oldName ++ " to " ++ newName)
  where
    createParent = createDirectoryIfMissing True . takeDirectory

-- cannot just remove and add again, because of the history
-- | Move a bookmark into a bookmark directory. The directory
-- will be created if not present already.
moveBookmark :: FilePath -> BookmarkName -> BookmarkName -> IO ()
moveBookmark baseDir oldName' newParent' = do
  let oldName = normalizePath oldName'
  let newParent = normalizePath newParent'
  withCurrentDirectory baseDir $ do
    whenM (not <$> doesPathExist oldName) $
      throw (HookmarkException $ "not found " ++ oldName)
    -- TODO error message if destination is an existing file
    createDirectoryIfMissing True newParent
    movePath oldName newParent
    cleanDirectory $ takeDirectory oldName
    whenM
      (isGitRepo ".git")
      (commitAll $ "move " ++ oldName ++ " to " ++ newParent)

movePath :: FilePath -> FilePath -> IO ()
movePath oldName' directory' = do
  let oldName = normalizePath oldName'
  let directory = normalizePath directory'
  ifM
    (not <$> doesDirectoryExist directory)
    (fail $ directory ++ " is no directory")
    (renamePath oldName (directory </> takeFileName oldName))

loadBookmarks :: FilePath -> IO [Bookmark]
loadBookmarks baseDir =
  ifM
    (doesDirectoryExist baseDir)
    (withCurrentDirectory baseDir $
     listDirectories "." >>= mapM readBookmark . sanitizePathList)
    (return [])
  where
    readBookmark :: FilePath -> IO Bookmark
    readBookmark name = do
      parsed <- parseBookmarkEntry <$> Text.readFile name
      case parsed of
        Right entry -> return (name, entry)
        Left err    -> fail $ Text.unpack err
    sanitizePathList :: [FilePath] -> [FilePath]
    sanitizePathList = dropGitDir . dropCurrentPathPrefix
    -- | drop "./"
    dropCurrentPathPrefix :: [FilePath] -> [FilePath]
    dropCurrentPathPrefix = fmap (drop 2)
    dropGitDir :: [FilePath] -> [FilePath]
    dropGitDir = filter $ (/= ".git") . take 4

loadBookmark :: FilePath -> BookmarkName -> IO Bookmark
loadBookmark baseDir name' = do
  let name = normalizePath name'
  let criteria =
        BookmarkCriteria
          {criteriaBookmarkName = Just name, criteriaTags = Nothing}
  bookmarks <- filter (matchesCriteria criteria) <$> loadBookmarks baseDir
  case bookmarks of
    [bookmark] -> return bookmark
    _          -> throw $ HookmarkException $ name ++ " not found"

newtype HookmarkException =
  HookmarkException
    { fromHookmarkException :: String
    }
  deriving newtype (Show)

instance Exception HookmarkException
