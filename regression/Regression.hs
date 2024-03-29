{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import Data.FileEmbed
import Data.Function
import Data.Functor
import qualified Data.String as String (fromString)
import System.Directory
import System.Directory.Extra (listFilesRecursive)
import System.Directory.Recursive
import System.Environment as Env
import System.Exit
import System.FilePath
import System.Process.Typed
import Test.Hspec
import Test.Hspec.Expectations.Diff
import Test.Hspec.Expectations.Process.Typed
import UnliftIO.Temporary (withSystemTempDirectory)

isExecutable :: FilePath -> IO Bool
isExecutable filePath = filePath & getPermissions <&> executable

lookupExecutable :: FilePath -> IO FilePath
lookupExecutable name = do
  executables <-
    listFilesRecursive "."
      >>= filterM isExecutable
      <&> filter ((==) name . takeBaseName)
  case executables of
    [exe] -> return exe
    [] -> error $ "no executable found: " ++ name
    _ -> error $ "ambigious executable: " ++ unwords executables

main :: IO ()
main =
  hspec $ do
    describe "hookmark" $
      beforeAll (lookupExecutable "hookmark") $ do
        describe "help" $ do
          it "should print expected output" $ \cmd ->
            proc cmd ["--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help.out"), mempty)
          it "should print expected output for add" $ \cmd ->
            proc cmd ["add", "--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help_add.out"), mempty)
          it "should print expected output for show" $ \cmd ->
            proc cmd ["show", "--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help_show.out"), mempty)
          it "should print expected output for edit" $ \cmd ->
            proc cmd ["edit", "--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help_edit.out"), mempty)
          it "should print expected output for git" $ \cmd ->
            proc cmd ["git", "--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help_git.out"), mempty)
          it "should print expected output for mv" $ \cmd ->
            proc cmd ["mv", "--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help_mv.out"), mempty)
          it "should print expected output for rm" $ \cmd ->
            proc cmd ["rm", "--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help_rm.out"), mempty)
        describe "add" $ do
          it "should create bookmark file at the correct place" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \baseDir _ -> do
              proc
                cmd
                [ "add",
                  "-t",
                  "search",
                  "--tag",
                  "haskell",
                  "haskell/hoogle",
                  "https://hoogle.haskell.org/"
                ]
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/hoogle") `shouldReturn` True
          it "should create missing base dir" $ \cmd ->
            withSystemTempDirectory "hookmark_test" $ \baseDir -> do
              removeDirectory baseDir
              doesDirectoryExist baseDir `shouldReturn` False
              proc
                cmd
                [ "add",
                  "-b",
                  baseDir,
                  "--tag",
                  "haskell",
                  "haskell/hoogle",
                  "https://hoogle.haskell.org/"
                ]
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesDirectoryExist baseDir `shouldReturn` True
          it "should prefer baseDir option over environment" $ \cmd ->
            withSystemTempDirectory "hookmark_test" $ \baseDir ->
              withSystemTempDirectory "hookmark_test" $ \tmpDir -> do
                removeDirectory baseDir
                doesDirectoryExist baseDir `shouldReturn` False
                proc
                  cmd
                  [ "add",
                    "-b",
                    tmpDir,
                    "--tag",
                    "haskell",
                    "haskell/hoogle",
                    "https://hoogle.haskell.org/"
                  ]
                  `shouldExit` (ExitSuccess, mempty, mempty)
                doesDirectoryExist baseDir `shouldReturn` False
                doesPathExist (tmpDir </> "haskell/hoogle") `shouldReturn` True
          it "should use hookmark root, not file system root" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \baseDir _ -> do
              proc
                cmd
                [ "add",
                  "-t",
                  "search",
                  "--tag",
                  "haskell",
                  "/haskell/hoogle",
                  "https://hoogle.haskell.org/"
                ]
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/hoogle") `shouldReturn` True
        describe "show" $ do
          it "should list bookmarks from correct place" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \baseDir _ ->
              (proc cmd . words $ "show -b " ++ baseDir)
                `shouldExit` (ExitSuccess, $(embedFile "regression/show_all.out"), mempty)
          it "should prefer baseDir option over environment" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \_ _ ->
              withSystemTempDirectory "hookmark_test" $ \baseDir ->
                (proc cmd . words $ "show -b " ++ baseDir)
                  `shouldExit` (ExitFailure 1, mempty, "no bookmark found\n")
          it "should list hookmark root, not file system root" $ \cmd -> do
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show /")
                `shouldExit` (ExitSuccess, $(embedFile "regression/show_all.out"), mempty)
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show /haskell/hoogle")
                `shouldExit` ( ExitSuccess,
                               $(embedFile "regression/testmarks/haskell/hoogle"),
                               mempty
                             )
          it "should list directories" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show haskell")
                `shouldExit` (ExitSuccess, $(embedFile "regression/show_haskell.out"), mempty)
          it "should return not successful if bookmark not found" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show missing/bookmark")
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/show_not_found.out")
                             )
          it "should return not successful if tag not found" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show -t missingTag")
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/show_not_found.out")
                             )
          it "should return not successful if tag missing in bookmark" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show -t missingTag haskell/hoogle")
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/show_not_found.out")
                             )
          it "should show bookmarks with name as prefix of another" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \_ _ ->
              (proc cmd . words $ "show haskell/packages/yesod")
                `shouldExit` ( ExitSuccess,
                               $(embedFile "regression/testmarks/haskell/packages/yesod"),
                               mempty
                             )
        describe "edit" $ do
          it "should edit correct file" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \baseDir _ -> do
              let editing = editBookmark cmd
              editing
                `shouldExit` (ExitSuccess, $(embedFile "regression/editor.out"), "")
              newContent <- BS.readFile (baseDir </> "haskell/hoogle")
              newContent `shouldNotDiffer` $(embedFile "regression/edit.out")
          it "should use hookmark root, not file system root" $ \cmd ->
            withTempHookmarks "hookmark_test" $ \baseDir _ -> do
              let editing = editBookmark cmd
              editing
                `shouldExit` (ExitSuccess, $(embedFile "regression/edit.out"), "")
              newContent <- BS.readFile (baseDir </> "haskell/hoogle")
              newContent `shouldNotDiffer` $(embedFile "regression/edit.out")
        describe "rm" $ do
          it "should remove correct file" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              (proc cmd . words $ "rm haskell/packages/warp")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
          it "should remove file empty folders" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              shouldExitSuccessfully
                (proc cmd . words $ "rm haskell/packages/base64-bytestring")
              shouldExitSuccessfully
                (proc cmd . words $ "rm haskell/packages/warp")
              shouldExitSuccessfully
                (proc cmd . words $ "rm haskell/packages/yesod")
              shouldExitSuccessfully
                (proc cmd . words $ "rm haskell/packages/yesod-core")
              doesPathExist (baseDir </> "haskell/packages")
                `shouldReturn` False
          it "should not remove empty base" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              removeFile $ baseDir </> ".git" </> "HEAD"
              removeDirectory $ baseDir </> ".git" </> "refs"
              removeDirectory $ baseDir </> ".git" </> "objects"
              removeDirectory $ baseDir </> ".git"
              (proc cmd . words $ "rm duckduckgo")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "rm haskell/haskell")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "rm haskell/hoogle")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "rm haskell/packages/base64-bytestring")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "rm haskell/packages/warp")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "rm haskell/packages/yesod")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "rm haskell/packages/yesod-core")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist baseDir `shouldReturn` True
              listDirectory baseDir `shouldReturn` []
          it "should use hookmark root, not file system root" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              (proc cmd . words $ "rm /duckduckgo")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "duckduckgo") `shouldReturn` False
        describe "mv" $ do
          it "should move files between directories" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              (proc cmd . words $ "mv haskell/packages/warp haskell")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
              doesPathExist (baseDir </> "haskell/warp") `shouldReturn` True
          it "should rename files" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              ( proc cmd . words $
                  "mv haskell/packages/warp haskell/packages/warp2"
                )
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
              doesPathExist (baseDir </> "haskell/packages/warp2")
                `shouldReturn` True
              newContent <- BS.readFile (baseDir </> "haskell/packages/warp2")
              newContent
                `shouldNotDiffer` $(embedFile "regression/testmarks/haskell/packages/warp")
          it "should overwrite existing files" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              ( proc cmd . words $
                  "mv haskell/packages/warp haskell/packages/base64-bytestring"
                )
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
              doesPathExist (baseDir </> "haskell/packages/base64-bytestring")
                `shouldReturn` True
              newContent <-
                BS.readFile (baseDir </> "haskell/packages/base64-bytestring")
              newContent
                `shouldNotDiffer` $(embedFile "regression/testmarks/haskell/packages/warp")
          it "shall remove empty folder" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              ( proc cmd . words $
                  "mv haskell/packages/base64-bytestring haskell"
                )
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "mv haskell/packages/warp haskell")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "mv haskell/packages/yesod haskell")
                `shouldExit` (ExitSuccess, mempty, mempty)
              (proc cmd . words $ "mv haskell/packages/yesod-core haskell")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages")
                `shouldReturn` False
          it "should use hookmark root, not file system root" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              (proc cmd . words $ "mv haskell/packages/warp /haskell")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
              doesPathExist (baseDir </> "haskell/warp") `shouldReturn` True
          it "can move to hookmark root" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              (proc cmd . words $ "mv haskell/packages/warp /")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
              doesPathExist (baseDir </> "warp") `shouldReturn` True
          it "shall move folders" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              (proc cmd . words $ "mv haskell/packages/ /")
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages")
                `shouldReturn` False
              doesPathExist (baseDir </> "packages") `shouldReturn` True
              doesPathExist (baseDir </> "packages/warp") `shouldReturn` True
              doesPathExist (baseDir </> "packages/base64-bytestring")
                `shouldReturn` True
          it "shall move list of files simultanously" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              ( proc cmd . words $
                  "mv haskell/packages/warp haskell/packages/base64-bytestring /"
                )
                `shouldExit` (ExitSuccess, mempty, mempty)
              doesPathExist (baseDir </> "haskell/packages/warp")
                `shouldReturn` False
              doesPathExist (baseDir </> "haskell/packages/base64-bytestring")
                `shouldReturn` False
              doesPathExist (baseDir </> "warp") `shouldReturn` True
              doesPathExist (baseDir </> "base64-bytestring")
                `shouldReturn` True
          it "shall report missing argument" $ \cmd ->
            withTempHookmarks "testmarks" $ \_ _ ->
              (proc cmd . words $ "mv")
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/mv_missing_arg.out")
                             )
          it "shall report missing destination" $ \cmd ->
            withTempHookmarks "testmarks" $ \_ _ ->
              (proc cmd . words $ "mv haskell/hoogle")
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/mv_missing_arg.out")
                             )
          it "shall report missing directory" $ \cmd ->
            withTempHookmarks "testmarks" $ \_ _ ->
              ( proc cmd . words $
                  "mv haskell/packages/warp duckduckgo haskell/packages/base64-bytestring"
              )
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/mv_missing_folder.out")
                             )
          it "shall report not existing destination" $ \cmd ->
            withTempHookmarks "testmarks" $ \_ _ ->
              ( proc cmd . words $
                  "mv haskell/packages/warp duckduckgo haskell/packages/base64-bytestring /not-existing"
              )
                `shouldExit` ( ExitFailure 1,
                               mempty,
                               $(embedFile "regression/mv_missing_folder_2.out")
                             )
          it
            "shall report missing files, if folder and content are moved at the same time"
            $ \cmd ->
              withTempHookmarks "testmarks" $ \baseDir _ -> do
                ( proc cmd . words $
                    "mv haskell/packages/warp haskell/packages haskell/packages/base64-bytestring haskell/hoogle /"
                  )
                  `shouldExit` ( ExitFailure 1,
                                 mempty,
                                 $(embedFile "regression/mv_missing_file.out")
                               )
                doesPathExist (baseDir </> "warp") `shouldReturn` True
                doesPathExist (baseDir </> "packages") `shouldReturn` True
                doesPathExist (baseDir </> "ackages/warp") `shouldReturn` False
                doesPathExist (baseDir </> "packages/base64-bytestring")
                  `shouldReturn` True
                doesPathExist (baseDir </> "haskell/hoogle") `shouldReturn` True
        describe "git" $ do
          it "should execute git from PATH" $ \cmd ->
            withTempHookmarks "testmarks" $ \_ gitLog -> do
              (proc cmd . words $ "git push")
                `shouldExit` (ExitSuccess, $(embedFile "regression/git.out"), "")
              gitOut <- BS.readFile gitLog
              gitOut `shouldNotDiffer` $(embedFile "regression/git.out")
          describe "should automatically commit" $ do
            it "add" $ \cmd ->
              withTempHookmarks "testmarks" $ \_ gitLog -> do
                ( proc cmd . words $
                    "add --tag haskell haskell/fpcomplete https://haskell.fpcomplete.com"
                  )
                  `shouldExit` (ExitSuccess, "", "")
                gitOut <- BS.readFile gitLog
                gitOut `shouldNotDiffer` $(embedFile "regression/git_add.out")
            it "edit" $ \cmd ->
              withTempHookmarks "testmarks" $ \_ gitLog -> do
                let editing = editBookmark cmd
                editing
                  `shouldExit` (ExitSuccess, $(embedFile "regression/editor.out"), "")
                gitOut <- BS.readFile gitLog
                gitOut `shouldNotDiffer` $(embedFile "regression/git_edit.out")
            it "rm" $ \cmd ->
              withTempHookmarks "testmarks" $ \_ gitLog -> do
                (proc cmd . words $ "rm duckduckgo")
                  `shouldExit` (ExitSuccess, "", "")
                gitOut <- BS.readFile gitLog
                gitOut `shouldNotDiffer` $(embedFile "regression/git_rm.out")
            it "mv" $ \cmd ->
              withTempHookmarks "testmarks" $ \_ gitLog -> do
                (proc cmd . words $ "mv duckduckgo duck")
                  `shouldExit` (ExitSuccess, "", "")
                gitOut <- BS.readFile gitLog
                gitOut `shouldNotDiffer` $(embedFile "regression/git_mv.out")
          it ".git should be hidden from show" $ \cmd ->
            withTempHookmarks "testmarks" $ \baseDir _ -> do
              createDirectory $ baseDir </> "haskell" </> ".git"
              writeFile
                (baseDir </> "haskell" </> ".git" </> "phony-git-file")
                ""
              (proc cmd . words $ "show")
                `shouldExit` (ExitSuccess, $(embedFile "regression/git_show.out"), "")
    describe "hookmark-web" $
      beforeAll (lookupExecutable "hookmark-web") $
        describe "help" $
          it "should print expected output" $ \cmd ->
            proc cmd ["--help"]
              `shouldExit` (ExitSuccess, $(embedFile "regression/help-web.out"), mempty)

withTempHookmarks :: String -> (FilePath -> FilePath -> IO ()) -> IO ()
withTempHookmarks nameTemplate action =
  withSystemTempDirectory nameTemplate $ \baseDir -> do
    let testmarksPath = baseDir </> "testmarks"
    copyDir "regression/testmarks" testmarksPath
    Env.setEnv "HOOKMARKHOME" testmarksPath
    createPhonyGitRepo testmarksPath
    cwd <- getCurrentDirectory
    Env.getEnv "PATH" >>= Env.setEnv "PATH" . ((cwd </> "regression:") ++)
    let gitlog = baseDir </> "git.log"
    Env.setEnv "GITLOG" gitlog
    Env.setEnv "EDITOR" "tee"
    action testmarksPath gitlog

editBookmark :: FilePath -> ProcessConfig () () ()
editBookmark cmd =
  setStdin
    (String.fromString $ BU.toString $(embedFile "regression/edit.in"))
    (proc cmd . words $ "edit haskell/hoogle")

-- | We cannot commit the phony git repo in a real git, therefor we have to
-- create it as part of the setup.
createPhonyGitRepo :: FilePath -> IO ()
createPhonyGitRepo dir = do
  createDirectory $ dir </> ".git"
  createDirectory $ dir </> ".git" </> "objects"
  createDirectory $ dir </> ".git" </> "refs"
  writeFile (dir </> ".git" </> "HEAD") ""
