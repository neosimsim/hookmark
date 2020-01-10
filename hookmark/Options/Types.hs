module Options.Types
  ( Options(..)
  ) where

import           Data.Text (Text)

data Options
  = Version
  | AddBookmark
      { addBaseDir     :: Maybe FilePath
      , addTags        :: [Text]
      , addDescription :: Bool
      , addName        :: FilePath
      , addUrl         :: Text
      }
  | ShowBookmark
      { showBaseDir :: Maybe FilePath
      , showTags    :: [Text]
      , showName    :: Maybe FilePath
      }
  | EditBookmark
      { editBaseDir :: Maybe FilePath
      , editName    :: FilePath
      }
  | RemoveBookmark
      { removeBaseDir :: Maybe FilePath
      , removeName    :: FilePath
      }
  | MoveBookmark
      { moveBaseDir     :: Maybe FilePath
      , moveSources     :: [FilePath] -- TODO NonEmptyList
      , moveDestination :: FilePath
      }
  | ExecBookmark
      { execBaseDir    :: Maybe FilePath
      , execCmdSources :: String
      , execArgs       :: [String]
      }
  deriving (Show)
