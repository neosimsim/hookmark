{-# LANGUAGE FlexibleContexts #-}

module Test.Hspec.Expectations.Process.Typed
  ( shouldExit,
    shouldExitSuccessfully,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Function (on)
import System.Exit
import System.Process.Typed
import Test.Hspec
import Test.Hspec.Expectations.Internal
import Text.Nicify

shouldExit ::
  HasCallStack =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  (ExitCode, BS.ByteString, BS.ByteString) ->
  Expectation
shouldExit p expectations = do
  results <- readProcess' p
  unless (expectations == results) . expectationFailure $
    (diff `on` nicify . prettyProcessResult) results expectations

shouldExitSuccessfully ::
  HasCallStack =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Expectation
shouldExitSuccessfully p = p `shouldExit` (ExitSuccess, mempty, mempty)

prettyProcessResult :: (ExitCode, BS.ByteString, BS.ByteString) -> String
prettyProcessResult (code, out, err) =
  unlines
    [ "ProcessResult:",
      "ExitCode:",
      show code,
      "Output:",
      show out,
      "Error Output:",
      show err
    ]

readProcess' ::
  MonadIO m =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  m (ExitCode, BS.ByteString, BS.ByteString)
readProcess' p = map2_3 toStrict <$> readProcess p
  where
    map2_3 :: (b -> c) -> (a, b, b) -> (a, c, c)
    map2_3 f (x, y, z) = (x, f y, f z)
