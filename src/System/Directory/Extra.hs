module System.Directory.Extra
  ( listDirectories
  , cleanDirectory
  ) where

import           Control.Monad                  ( join )
import           Control.Monad.Extra            ( whenM )
import           System.Directory               ( doesDirectoryExist
                                                , doesPathExist
                                                , listDirectory
                                                , removeDirectory
                                                )
import           System.FilePath                ( joinPath
                                                , takeDirectory
                                                )

cleanDirectory :: FilePath -> IO ()
cleanDirectory "." = return ()
cleanDirectory path =
  whenM (doesDirectoryExist path) $ whenM (null <$> listDirectory path) $ do
    removeDirectory path
    cleanDirectory (takeDirectory path)

listDirectories :: FilePath -> IO [FilePath]
listDirectories ""   = fmap (drop 2) <$> listDirectories "."
listDirectories path = do
  exists <- doesPathExist path
  if exists
    then do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          files <- listDirectory path
          x     <- mapM listDirectories $ fmap (\x -> joinPath [path, x]) files
          return $ join x
        else return [path]
    else return []
