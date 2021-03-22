module Test.QuickCheck.Gen.Extra
  ( arbitraryText
  , arbitraryLine
  , arbitraryNonEmptyLine
  ) where

import           Data.NonEmptyText              ( NonEmptyText )
import qualified Data.NonEmptyText             as NonEmptyText
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Test.QuickCheck                ( Gen
                                                , arbitrary
                                                )

arbitraryText :: Gen Text
arbitraryText = Text.pack <$> arbitrary

arbitraryLine :: Gen Text
arbitraryLine = Text.pack . filter (/= '\n') <$> arbitrary

arbitraryNonEmptyLine :: Gen NonEmptyText
arbitraryNonEmptyLine = do
  t <- arbitraryLine
  maybe arbitraryNonEmptyLine return (NonEmptyText.fromText t)
