module Main
  ( main
  ) where

import           Hookmark.Web
import           System.Environment (getEnv)
import           System.FilePath    ((</>))
import           Yesod

main :: IO ()
main = do
  home <- getEnv "HOME"
  warp 3001 HookmarkWeb {hookmarkWebBaseDir = home </> ".hookmarks"}
