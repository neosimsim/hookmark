module Main
  ( main
  ) where

import           Hookmark.Web
import           Yesod

main :: IO ()
main = warp 3001 HookmarkWeb
