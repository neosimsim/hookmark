{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Hookmark.Web
  ( HookmarkWeb(..)
  , resourcesHookmarkWeb
  ) where

import           Control.Monad     (when)
import           Data.List         (sort)
import           Data.Maybe        (catMaybes)
import qualified Data.NonEmptyText as NonEmptyText (fromText, toText)
import           Data.Text         (Text)
import qualified Data.Text         as Text (pack, unpack)
import           Data.Version      (showVersion)
import           Hookmark.IO
import           Hookmark.Types
import           Paths_hookmark    (version)
import           System.FilePath   as FilePath (joinPath, splitDirectories)
import           Text.Hamlet
import           Yesod

newtype HookmarkWeb =
  HookmarkWeb
    { hookmarkWebBaseDir :: FilePath
    }

mkYesod
  "HookmarkWeb"
  [parseRoutes|
/ HomeR GET
/view/+[FilePath] ViewR GET
/list/+[FilePath] ListR GET
/edit/+[FilePath] EditR GET POST
|]

instance Yesod HookmarkWeb where
  defaultLayout = myLayout

instance RenderMessage HookmarkWeb FormMessage where
  renderMessage _ _ = defaultFormMessage

type HookmarkWebRoute = Route HookmarkWeb

myLayout :: Widget -> Handler Html
myLayout widget = do
  pc <- widgetToPageContent widget
  HookmarkWeb {..} <- getYesod
  withUrlRenderer
    [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    <style>body { font-family: verdana }
                    ^{pageHead pc}
                <body>
                    ^{header}
                    <article>
                        ^{pageBody pc}
                    ^{footer (showVersion version)}
        |]

data BookmarkFormData =
  BookmarkFormData
    { bookmarkFormName        :: Text
    , bookmarkFormUrl         :: Text
    , bookmarkFormTags        :: [Text]
    , bookmarkFormDescription :: Maybe Textarea
    }
  deriving (Show)

emptyBookmarkFormData :: BookmarkFormData
emptyBookmarkFormData =
  BookmarkFormData
    { bookmarkFormName = ""
    , bookmarkFormUrl = ""
    , bookmarkFormTags = []
    , bookmarkFormDescription = Nothing
    }

bookmarkToFormData :: Bookmark -> BookmarkFormData
bookmarkToFormData (name, BookmarkEntry {..}) =
  BookmarkFormData
    { bookmarkFormName = Text.pack name
    , bookmarkFormUrl = url
    , bookmarkFormTags = NonEmptyText.toText <$> tags
    , bookmarkFormDescription = Just (Textarea description)
    }

formDataToBookmark :: BookmarkFormData -> Bookmark
formDataToBookmark BookmarkFormData {..} =
  ( Text.unpack bookmarkFormName
  , BookmarkEntry
      { url = bookmarkFormUrl
      , tags = catMaybes $ NonEmptyText.fromText <$> bookmarkFormTags
      , description = maybe "" unTextarea bookmarkFormDescription
      })

getHomeR :: Handler ()
getHomeR = redirect $ ListR []

getViewR :: [FilePath] -> Handler Html
getViewR paths = do
  let path = FilePath.joinPath paths
  let criteria =
        BookmarkCriteria
          { criteriaBookmarkName =
              if null path
                then Nothing
                else Just path
          , criteriaTags = Nothing
          }
  HookmarkWeb {..} <- getYesod
  bookmarks <-
    liftIO $
    filter (matchesCriteria criteria) <$> loadBookmarks hookmarkWebBaseDir
  case bookmarks of
    [bookmark] -> defaultLayout $ viewBookmarkWidget bookmark
    _          -> notFound

getListR :: [FilePath] -> Handler Html
getListR paths = do
  let path = FilePath.joinPath paths
  getParams <- reqGetParams <$> getRequest
  let tags =
        catMaybes $
        NonEmptyText.fromText . snd <$> filter ((== "tag") . fst) getParams
  let criteria =
        BookmarkCriteria
          { criteriaBookmarkName =
              if null path
                then Nothing
                else Just path
          , criteriaTags =
              if null tags
                then Nothing
                else Just tags
          }
  HookmarkWeb {..} <- getYesod
  bookmarks <-
    liftIO $
    filter (matchesCriteria criteria) <$> loadBookmarks hookmarkWebBaseDir
  defaultLayout . viewFilePathsWidget $ bookmarkName <$> bookmarks

getEditR :: [FilePath] -> Handler Html
getEditR paths = do
  let path = FilePath.joinPath paths
  bookmarkFormData <-
    if null path
      then return emptyBookmarkFormData
      else do
        HookmarkWeb {..} <- getYesod
        liftIO $ bookmarkToFormData <$> loadBookmark hookmarkWebBaseDir path
  (formWidget, enctype) <- generateFormPost $ bookmarkForm bookmarkFormData
  defaultLayout $ editBookmarkWidget formWidget enctype

postEditR :: [FilePath] -> Handler Html
postEditR paths = do
  HookmarkWeb {..} <- getYesod
  ((result, _), _) <- runFormPostNoToken $ bookmarkForm emptyBookmarkFormData
  case result of
    FormSuccess bookmarkData -> do
      formCommand <- lookupPostParam "command"
      case formCommand of
        Just "addtag" -> do
          let bookmarkDataWithNewTag =
                bookmarkData
                  {bookmarkFormTags = "" : bookmarkFormTags bookmarkData}
          (formWidget, enctype) <-
            generateFormPost $ bookmarkForm bookmarkDataWithNewTag
          defaultLayout $ editBookmarkWidget formWidget enctype
        _ -> do
          let newBookmark@(newName, _) = formDataToBookmark bookmarkData
          let oldName = FilePath.joinPath paths
          when
            (not (null oldName) && oldName /= newName)
            (liftIO $ renameBookmark hookmarkWebBaseDir oldName newName)
          liftIO $ saveBookmark hookmarkWebBaseDir newBookmark
          redirect . ViewR $ splitDirectories newName
    FormFailure msgs ->
      defaultLayout
        [whamlet|
      <p>Invalid input, let's try again.
      $forall msg <- msgs
        #{msg}
    |]
    FormMissing ->
      defaultLayout [whamlet| <p>Invalid input, let's try again.  |]

viewFilePathsWidget :: [FilePath] -> Widget
viewFilePathsWidget filePaths = do
  setTitle "HookmarkWeb – list"
  $(whamletFile "templates/list.hamlet")

viewBookmarkWidget :: Bookmark -> Widget
viewBookmarkWidget bookmark = do
  setTitle . toHtml $ "HookmarkWeb – " ++ bookmarkName bookmark
  $(whamletFile "templates/view.hamlet")

editBookmarkWidget :: Widget -> Enctype -> Widget
editBookmarkWidget formWidget enctype = do
  setTitle "HookmarkWeb – edit"
  $(whamletFile "templates/edit.hamlet")

header :: HtmlUrl HookmarkWebRoute
header = $(hamletFile "templates/header.hamlet")

footer :: String -> HtmlUrl HookmarkWebRoute
footer versionStr = $(hamletFile "templates/footer.hamlet")

bookmarkForm ::
     BookmarkFormData
  -> Html
  -> MForm Handler (FormResult BookmarkFormData, Widget)
bookmarkForm BookmarkFormData {..} _ = do
  (nameRes, nameView) <- mreq textField "Name" (Just bookmarkFormName)
  (urlRes, urlView) <- mreq urlField "Url" (Just bookmarkFormUrl)
  (tagsRes, tagsView) <- mreq tagsField "Tags" (Just bookmarkFormTags)
  (descriptionRes, descriptionView) <-
    mopt textareaField "Description" (Just bookmarkFormDescription)
  let bookmarkRes =
        BookmarkFormData <$> nameRes <*> urlRes <*> tagsRes <*> descriptionRes
  let widget = $(whamletFile "templates/bookmarkForm.hamlet")
  return (bookmarkRes, widget)

tagsField :: Field Handler [Text]
tagsField =
  Field
    { fieldParse = \rawVals _ -> return $ Right $ Just rawVals
    , fieldView =
        \idAttr nameAttr otherAttrs eResult _ ->
          case eResult of
            Left invalid ->
              [whamlet|
                <p>Invalid input, let's try again.
                #{invalid}
              |]
            Right tagsResult ->
              [whamlet|
                $forall (i, tag) <- withIndex $ sort tagsResult
                  <input id=#{idAttr}#{i} name=#{nameAttr} *{otherAttrs} type=text value=#{tag}>
              |]
    , fieldEnctype = UrlEncoded
    }
  where
    withIndex :: [a] -> [(Int, a)]
    withIndex = zip [0,1 ..]
