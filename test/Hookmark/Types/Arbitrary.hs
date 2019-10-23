{-# OPTIONS_GHC -Wno-orphans #-}

module Hookmark.Types.Arbitrary
  ( BookmarkEntry
  ) where

import           Hookmark.Types            (BookmarkEntry (..))
import           Test.QuickCheck           (Arbitrary (..), listOf)
import           Test.QuickCheck.Gen.Extra (arbitraryLine,
                                            arbitraryNonEmptyLine,
                                            arbitraryText)

instance Arbitrary BookmarkEntry where
  arbitrary =
    BookmarkEntry <$> arbitraryLine <*> listOf arbitraryNonEmptyLine <*>
    arbitraryText
