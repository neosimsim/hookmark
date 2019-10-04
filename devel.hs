{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.Async             (race_)
import           "hookmark" Hookmark.Web              (HookmarkWeb (..))
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           System.Directory                     (doesFileExist)
import           System.Environment
import           System.FilePath
import           Yesod

main :: IO ()
main = develMain

develMain :: IO ()
develMain = do
  race_ watchTermFile $ do
    port <- read <$> getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putStrLn $ "Running in development mode on port " ++ show port
    putStrLn $ "But you should connect to port " ++ displayPort
    app <- toWaiApp HookmarkWeb {hookmarkWebBaseDir = "regression/testmarks"}
    run port $ logStdoutDev app

-- | Would certainly be more efficient to use fsnotify, but this is
-- simpler.
watchTermFile :: IO ()
watchTermFile = loop
  where
    loop = do
      exists <- doesFileExist "yesod-devel/devel-terminate"
      if exists
        then return ()
        else do
          threadDelay 100000
          loop
