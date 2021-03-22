module Data.Path
  ( toRelativeFilePath,
  )
where

import System.FilePath

toRelativeFilePath :: FilePath -> FilePath
toRelativeFilePath "/" = "."
toRelativeFilePath filePath =
  dropTrailingPathSeparator . snd $ splitDrive filePath
