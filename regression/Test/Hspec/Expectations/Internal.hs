module Test.Hspec.Expectations.Internal
  ( diff
  ) where

import           Data.Algorithm.Diff (Diff (..), getDiff)
import           Data.Function       (on)

diff :: String -> String -> String
diff x y = unlines $ addSign <$> (getDiff `on` lines) x y
  where
    addSign (Both _ s) = "   " ++ s
    addSign (First s)  = "---" ++ s
    addSign (Second s) = "+++" ++ s
