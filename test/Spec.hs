{-# LANGUAGE OverloadedStrings #-}

import           Data.Text
import           Lib
import           Test.Hspec
import           Test.QuickCheck

newtype TText =
  TText
    { fromTText :: Text
    }

instance Arbitrary TText where
  arbitrary = fmap (TText . pack) arbitrary

arbitraryText :: Gen Text
arbitraryText = fromTText <$> arbitrary

newtype TBookmark =
  TBookmark
    { fromTBookmark :: Bookmark
    }
  deriving (Eq, Show)

instance Arbitrary TBookmark where
  arbitrary =
    fmap TBookmark $
    Bookmark <$> arbitraryText <*> arbitrary <*> listOf arbitraryText <*>
    arbitraryText

main :: IO ()
main =
  hspec $
  describe "Lib.parse/renderBookmark" $
  it "parses rendered data the same" $
  property $ \x ->
    x === (TBookmark . parseBookmark . renderBookmark $ fromTBookmark x)
