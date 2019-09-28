{-# LANGUAGE TemplateHaskell #-}

module Distribution.Git
  ( compileAbbrHash
  ) where

import           Control.Exception
import           Language.Haskell.TH
import           System.Directory
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
safeGetGitHash =
  catch
    (Just <$> readProcess "git" ["log", "-1", "--pretty=format:%h"] [])
    handleEverything
  where
    handleEverything :: SomeException -> IO (Maybe String)
    handleEverything _ = return Nothing

gitFiles :: [FilePath]
gitFiles = ["HEAD"]

gitDirectories :: [FilePath]
gitDirectories = ["refs", "objects"]

isGitRepo :: FilePath -> IO Bool
isGitRepo repo = do
  repoExists <- doesDirectoryExist repo
  if repoExists
    then withCurrentDirectory repo $
         and <$>
         mconcat
           [mapM doesDirectoryExist gitDirectories, mapM doesFileExist gitFiles]
    else return False
