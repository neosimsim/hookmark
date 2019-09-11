{-# LANGUAGE FlexibleContexts #-}

module Test.Hspec.Expectations.Diff
  ( shouldNotDiffer
  ) where

import           Control.Monad
import           Data.Function                    (on)
import           Test.Hspec.Expectations
import           Test.Hspec.Expectations.Internal
import           Test.HUnit
import           Text.Nicify

shouldNotDiffer :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
a `shouldNotDiffer` b =
  unless (a == b) . expectationFailure $ (diff `on` nicify . show) a b
