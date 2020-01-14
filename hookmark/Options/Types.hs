module Options.Types
  ( Options(..)
  , SubCommand(..)
  ) where

import           Data.Text (Text)

data Options
  = Version
  | Options
      { optionsBaseDir :: Maybe FilePath
      , subCommand     :: SubCommand
      }

data SubCommand
  = VersionBookmark
  | AddBookmark
      { optBookmarkTags      :: [Text]
      , optPromptDescription :: Bool
      , optBookmarkName      :: FilePath
      , optUrl               :: Text
      }
  | ShowBookmark
      { optBookmarkTags       :: [Text]
      , optBookmarkLookupName :: Maybe FilePath
      }
  | EditBookmark
      { optBookmarkName :: FilePath
      }
  | RemoveBookmark
      { optBookmarkName :: FilePath
      }
  | MoveBookmark
      { moveSources     :: [FilePath] -- TODO NonEmptyList
      , moveDestination :: FilePath
      }
  | ExecBookmark
      { execCmdSources :: String
      , execArgs       :: [String]
      }
  deriving (Show)
