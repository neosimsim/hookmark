module Hookmark.Types
  ( Tag
  , Bookmark
  , BookmarkEntry(..)
  , bookmarkName
  , renderBookmarkEntry
  ) where

import           Data.NonEmptyText
import           Data.Text         (Text)
import qualified Data.Text         as T

type Tag = NonEmptyText

type Bookmark = (FilePath, BookmarkEntry)

bookmarkName :: Bookmark -> FilePath
bookmarkName = fst

data BookmarkEntry =
  BookmarkEntry
    { url         :: Text
    , tags        :: [Tag]
    , description :: Text
    }
  deriving (Show, Eq)

renderBookmarkEntry :: BookmarkEntry -> Text
renderBookmarkEntry bm =
  T.concat
    [T.unlines ([url bm] ++ fmap toText (tags bm) ++ [mempty]), description bm]
