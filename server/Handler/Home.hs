{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Home where

import BasicPrelude
import Data.FileEmbed (embedFile)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Search (replace)
import Data.Text.Internal.Builder (toLazyText)
import Foundation
import System.Random (randomIO)
import Text.Julius (juliusFile, rawJS, renderJavascriptUrl)
import Yesod.Core
import Handler.Resources (staticResourcesExp, randStringExp)
import Model.Vocab
import Model.VocabSerialisation()
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text.Lazy as TL

getHomeR :: Handler Html
getHomeR = do
  lAvailable <- getLanguagesAvailable
  let clientState = InitData { languagesAvailable = lAvailable }
  defaultLayout $ do
  addScript $ StaticR js_all_js
  -- addScript $ StaticR js_all2_js
  setTitle "Ulpan Drill"
  toWidget
    [julius|
      window.addEventListener('js_init_ready', function() {
        js_init(#{toJSON clientState});
      });
    |]
  [whamlet|
    <div id="status">
    <div id="content">
  |]

-- TODO can do this in routes, e.g. /  RedirectR Home2R   ?
getHome2R :: Handler Html
getHome2R = redirect HomeR

getServiceWorkerR :: Handler TypedContent
getServiceWorkerR = do
  urlRender       <- getUrlRender
  urlRenderParams <- getUrlRenderParams
  App { aLanguageFiles = languageFiles } <- getYesod
  let filesToCacheSync = [ urlRender HomeR
                         , urlRender LanguagesAvailableR
                         , urlRender ManifestR
                         ]
                       <> $(staticResourcesExp "static")
      filesToCacheAsync = fmap (urlRender . LanguageFileR) $ M.keys languageFiles
      cacheName = "ulpan-drill-" <> ($(randStringExp) :: Text)
      serviceWorker = renderJavascriptUrl urlRenderParams $(juliusFile "templates/service-worker.julius")
  return $ TypedContent "application/javascript" $ toContent serviceWorker

getManifestR :: Handler Value
getManifestR = do
  urlRender <- getUrlRender
  return $ object
    [ "name"       .= ("Ulpan Drill" :: Text)
    , "short_name" .= ("Ulpan Drill" :: Text)
    , "icons"      .= Array (V.fromList
        [ object [ "src"   .= urlRender (StaticR img_icons_favicon_icons_1_Desktop_Icons_icon_128_png)
                 , "sizes" .= ("128x128"   :: Text)
                 , "type"  .= ("image/png" :: Text)
                 ]
        , object [ "src"   .= urlRender (StaticR img_icons_favicon_icons_1_Desktop_Icons_icon_256_png)
                 , "sizes" .= ("256x256"   :: Text)
                 , "type"  .= ("image/png" :: Text)
                 ]
        , object [ "src"   .= urlRender (StaticR img_icons_favicon_icons_1_Desktop_Icons_icon_512_png)
                 , "sizes" .= ("512x512"   :: Text)
                 , "type"  .= ("image/png" :: Text)
                 ]
        ])
    , "start_url"        .= urlRender HomeR
    , "display"          .= ("standalone" :: Text)
    , "background_color" .= ("#ffffff"    :: Text)
    , "theme_color"      .= ("#E09D5A"    :: Text)
    ]

getLanguageFileR :: Text -> Handler Value
getLanguageFileR languageFile = do
  App { aLanguageFiles = languageFiles } <- getYesod
  case M.lookup languageFile languageFiles of
    Just languageFiles -> return $ toJSON languageFiles
    Nothing            -> notFound

getLanguagesAvailable :: Handler [(Text, Text)]
getLanguagesAvailable = do
  App { aLanguageFiles = languageFiles } <- getYesod
  return $ (id *** name . lfMeta) <$> M.toList languageFiles

getLanguagesAvailableR :: Handler Value
getLanguagesAvailableR =
  getLanguagesAvailable >>= return . toJSON
