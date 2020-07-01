module System.Editor
  ( editFile
  )
where

import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( ExitCode(..)
                                                , exitWith
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Process.Typed           ( proc
                                                , runProcess
                                                )

-- | Opens the given file in an editor.
-- The editor is read from the environment variable EDITOR.
-- ed the default editor.
-- If anything fails, exits with ExitCode 1.
editFile :: FilePath -> IO ()
editFile filePath = do
  editor <- fromMaybe "ed" <$> lookupEnv "EDITOR"
  code   <- runProcess $ proc editor [filePath]
  case code of
    ExitFailure c -> do
      hPutStrLn stderr $ "editor failed: " ++ show c
      exitWith $ ExitFailure 1
    _ -> return ()
