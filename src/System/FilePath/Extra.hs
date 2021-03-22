module System.FilePath.Extra
  ( breadcrumbs
  ) where

breadcrumbs :: [FilePath] -> [[FilePath]]
breadcrumbs []       = []
breadcrumbs (x : xs) = [x] : ((x :) <$> breadcrumbs xs)
