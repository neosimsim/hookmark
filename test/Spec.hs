{-# LANGUAGE OverloadedStrings #-}

import           Data.NonEmptyText
import           Data.Text         as T (Text)
import qualified Data.Text         as T
import           Lib
import           Test.Hspec
import           Test.QuickCheck

newtype TBookmarkEntry =
  TBookmarkEntry
    { fromTBookmarkEntry :: BookmarkEntry
    }
  deriving (Eq, Show)

instance Arbitrary TBookmarkEntry where
  arbitrary =
    fmap TBookmarkEntry $
    BookmarkEntry <$> arbitraryLine <*> listOf arbitraryNonEmptyLine <*>
    arbitraryText

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryLine :: Gen Text
arbitraryLine = T.pack . filter (/= '\n') <$> arbitrary

arbitraryNonEmptyLine :: Gen NonEmptyText
arbitraryNonEmptyLine = do
  t <- arbitraryLine
  case fromText t of
    Just t' -> return t'
    Nothing -> arbitraryNonEmptyLine

prop_renderAndParserAreInverse :: TBookmarkEntry -> Property
prop_renderAndParserAreInverse x =
  (Right $ fromTBookmarkEntry x) ===
  (parseBookmarkEntry . renderBookmarkEntry $ fromTBookmarkEntry x)

main :: IO ()
main =
  hspec $
  describe "Lib.parse/renderBookmark" $
  it "parses rendered data the same" $ property prop_renderAndParserAreInverse
