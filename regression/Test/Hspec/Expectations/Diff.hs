{-# LANGUAGE FlexibleContexts #-}

module Test.Hspec.Expectations.Diff
  ( shouldNotDiffer
  ) where

import           Control.Monad
import           Data.Algorithm.Diff     (Diff (..), getDiff)
import           Data.Function           (on)
import           Test.Hspec.Expectations
import           Test.HUnit
import           Text.Nicify

shouldNotDiffer :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
a `shouldNotDiffer` b =
  unless (a == b) . expectationFailure $ (diff `on` nicify . show) a b

diff :: String -> String -> String
diff x y = unlines $ addSign <$> (getDiff `on` lines) x y
  where
    addSign (Both _ s) = "   " ++ s
    addSign (First s)  = "---" ++ s
    addSign (Second s) = "+++" ++ s
