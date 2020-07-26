{-# OPTIONS_GHC -Wno-orphans #-}

module Hookmark.IO.Arbitrary
  ( BookmarkCriteria
  )
where

import           Hookmark.IO
                 ( BookmarkCriteria(..) )
import           Test.QuickCheck.Gen.Extra
                 ( arbitraryNonEmptyLine )

import           Test.QuickCheck
                 ( Arbitrary(..)
                 , listOf
                 , oneof
                 )

instance Arbitrary BookmarkCriteria where
  arbitrary = do
    nameCrit <- arbitrary
    tagsCrit <- oneof [return Nothing, Just <$> listOf arbitraryNonEmptyLine]
    return $ BookmarkCriteria nameCrit tagsCrit
