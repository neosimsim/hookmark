{-# LANGUAGE TemplateHaskell #-}

module Distribution.Git
  ( compileAbbrHash
  ) where

import           Control.Exception
import           Git
import           Language.Haskell.TH
import           System.Process

compileAbbrHash :: Q Exp
compileAbbrHash = do
  isGit <- runIO $ isGitRepo ".git"
  if isGit
    then do
      hash <- runIO safeGetGitHash
      [|hash :: Maybe String|]
    else [|Nothing :: Maybe String|]

safeGetGitHash :: IO (Maybe String)
safeGetGitHash = catch
  (Just <$> readProcess "git" ["log", "-1", "--pretty=format:%h"] [])
  handleEverything
 where
  handleEverything :: SomeException -> IO (Maybe String)
  handleEverything _ = return Nothing
