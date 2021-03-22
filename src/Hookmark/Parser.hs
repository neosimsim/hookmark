module Hookmark.Parser
  ( parseBookmarkEntry
  ) where

import           Control.Monad
import           Data.Either.Combinators
import qualified Data.NonEmptyText             as T
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Hookmark.Types
import           Text.Megaparsec               as MP
import           Text.Megaparsec.Char          as MP

parseBookmarkEntry :: Text -> Either Text BookmarkEntry
parseBookmarkEntry =
  mapLeft (T.pack . errorBundlePretty) . parse bookmarkEntryParser ""

type Parser = Parsec Void Text

bookmarkEntryParser :: Parser BookmarkEntry
bookmarkEntryParser =
  BookmarkEntry
    .   T.pack
    <$> manyTill anySingle parseNewLineOrEnd
    <*> manyTill parseTag  parseNewLineOrEnd
    <*> (T.pack <$> manyTill anySingle eof)

parseTag :: Parser Tag
parseTag = T.new <$> anySingle <*> (T.pack <$> manyTill anySingle newline)

parseNewLineOrEnd :: Parser ()
parseNewLineOrEnd = MP.try (void newline) <|> eof
