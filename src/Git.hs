module Git
  ( isGitRepo
  , commitAll
  )
where

import qualified Data.ByteString.Lazy.UTF8
              as BLU
import qualified Data.Text
              as Text
                 ( pack )
import qualified Data.Text.IO
              as Text
                 ( hPutStrLn )
import           System.Directory
                 ( doesDirectoryExist
                 , doesFileExist
                 , withCurrentDirectory
                 )
import           System.Exit
                 ( ExitCode(..)
                 , exitWith
                 )
import           System.IO
                 ( stderr )
import           System.Process.Typed
                 ( proc
                 , readProcess
                 )

commitAll :: String -> IO ()
commitAll msg = do
  (addCode, _, addErr) <- readProcess $ proc "git" ["add", "."]
  case addCode of
    ExitFailure cadd -> do
      Text.hPutStrLn stderr . Text.pack $ concat
        ["auto add failed (", show cadd, ")", ": ", BLU.toString addErr]
      exitWith $ ExitFailure 1
    ExitSuccess -> do
      (commitCode, _, commitErr) <- readProcess
        $ proc "git" ["commit", "-m", msg]
      case commitCode of
        ExitFailure ccommit -> do
          Text.hPutStrLn stderr
            . Text.pack
            $ concat
                [ "auto commit failed ("
                , show ccommit
                , ")"
                , ": "
                , BLU.toString commitErr
                ]
          exitWith $ ExitFailure 1
        ExitSuccess -> return ()

gitFiles :: [FilePath]
gitFiles = ["HEAD"]

gitDirectories :: [FilePath]
gitDirectories = ["refs", "objects"]

-- | Checks if the given filepath exists and  contains the file HEAD and the
-- directories refs and objects.
isGitRepo :: FilePath -> IO Bool
isGitRepo repo = do
  repoExists <- doesDirectoryExist repo
  if repoExists
    then withCurrentDirectory repo $ and <$> mconcat
      [mapM doesDirectoryExist gitDirectories, mapM doesFileExist gitFiles]
    else return False
