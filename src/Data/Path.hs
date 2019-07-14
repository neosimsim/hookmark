module Data.Path
  ( normalizePath
  ) where

import           System.FilePath

normalizePath :: FilePath -> FilePath
normalizePath = dropTrailingPathSeparator . snd . splitDrive
