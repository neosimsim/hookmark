{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Hookmark.Web
  ( HookmarkWeb(..)
  , resourcesHookmarkWeb
  ) where

import           Yesod

data HookmarkWeb =
  HookmarkWeb

mkYesod
  "HookmarkWeb"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod HookmarkWeb

getHomeR :: Handler Html
getHomeR = defaultLayout homeWidget

homeWidget :: Widget
homeWidget =
  [whamlet|
$doctype 5
<p>
  Hello Hookmarks!
  <a href=@{HomeR}>Home
|]
