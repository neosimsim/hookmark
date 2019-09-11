{-# LANGUAGE FlexibleContexts #-}

module Test.Hspec.Expectations.Process.Typed
  ( shouldExit
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Algorithm.Diff    (Diff (..), getDiff)
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (toStrict)
import           Data.Function          (on)
import           System.Exit
import           System.Process.Typed
import           Test.Hspec
import           Text.Nicify

shouldExit ::
     HasCallStack
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> (ExitCode, BS.ByteString, BS.ByteString)
  -> Expectation
shouldExit p expectations = do
  results <- readProcess' p
  unless (expectations == results) . expectationFailure $
    (diff `on` nicify . prettyProcessResult) results expectations

prettyProcessResult :: (ExitCode, BS.ByteString, BS.ByteString) -> String
prettyProcessResult (code, out, err) =
  unlines
    [ "ProcessResult:"
    , "ExitCode:"
    , show code
    , "Output:"
    , show out
    , "Error Output:"
    , show err
    ]

readProcess' ::
     MonadIO m
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> m (ExitCode, BS.ByteString, BS.ByteString)
readProcess' p = map2_3 toStrict <$> readProcess p
  where
    map2_3 :: (b -> c) -> (a, b, b) -> (a, c, c)
    map2_3 f (x, y, z) = (x, f y, f z)

diff :: String -> String -> String
diff x y = unlines $ addSign <$> (getDiff `on` lines) x y
  where
    addSign (Both _ s) = "   " ++ s
    addSign (First s)  = "---" ++ s
    addSign (Second s) = "+++" ++ s
