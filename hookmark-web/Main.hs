{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Control.Applicative            ( (<|>) )
import           Data.Maybe                     ( fromMaybe )
import           Hookmark.Web                  as Web
import           Options                        ( Options(..)
                                                , parseOptions
                                                )
import           System.Environment             ( getEnv
                                                , lookupEnv
                                                )
import           System.FilePath                ( (</>) )
import           Yesod

main :: IO ()
main = do
  Options {..} <- parseOptions
  home         <- getEnv "HOME"
  hookmarkHome <- lookupEnv "HOOKMARKHOME"
  let baseDir =
        fromMaybe (home </> ".hookmarks") $ hookmarkWebBaseDir <|> hookmarkHome
  warp hookmarkWebPort HookmarkWeb { Web.hookmarkWebBaseDir = baseDir }
