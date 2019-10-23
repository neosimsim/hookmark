{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Hookmark.IO              (BookmarkCriteria (..),
                                           matchesCriteria)
import           Hookmark.Parser
import           Hookmark.Types
import           Hookmark.Types.Arbitrary (BookmarkEntry)
import           System.FilePath          (dropDrive, (</>))
import           Test.Hspec
import           Test.QuickCheck

prop_renderAndParserAreInverse :: BookmarkEntry -> Property
prop_renderAndParserAreInverse x =
  Right x === parseBookmarkEntry (renderBookmarkEntry x)

prop_bookmarkMatchesItself :: Bookmark -> Property
prop_bookmarkMatchesItself bookmark@(name', BookmarkEntry {tags = tags'}) =
  checkCoverage $
  cover 1 (null tags') "untagged" $
  cover 1 (not $ null tags') "tagged" $
  matchesCriteria (BookmarkCriteria (Just name') (Just tags')) bookmark

prop_bookmarkMatchesPathPrefix :: Bookmark -> FilePath -> Property
prop_bookmarkMatchesPathPrefix (name', entry) prefix
  -- we drop the drive to make sure the path is relative.
  -- Once typed paths are uses this should not be necessary:
  -- https://hackage.haskell.org/package/path
 =
  let bookmark' = (prefix </> dropDrive name', entry)
   in checkCoverage $
      cover 1 (null prefix) "exact name" $
      cover 1 (not $ null prefix) "subpath" $
      matchesCriteria (BookmarkCriteria (Just prefix) Nothing) bookmark'

prop_bookmarkMatchesTags :: Bookmark -> Property
prop_bookmarkMatchesTags bookmark@(name', BookmarkEntry {tags = tags'}) =
  checkCoverage $
  cover 1 (null tags') "untagged" $
  cover 1 (not $ null tags') "tagged" $
  matchesCriteria (BookmarkCriteria (Just name') (Just tags')) bookmark

main :: IO ()
main =
  hspec $ do
    describe
      "Hookmark.Parser.parseBookmarkEntry/Hookmark.Types.renderBookmarkEntry" $
      it "parses rendered data the same" $
      property prop_renderAndParserAreInverse
    describe "Hookmark.IO.matchesCriteria" $ do
      it "bookmarks should match themselve" $
        property prop_bookmarkMatchesItself
      it "bookmarks should match sublist of tags" $
        property prop_bookmarkMatchesTags
      it "bookmarks should match patch prefix" $
        property prop_bookmarkMatchesPathPrefix
