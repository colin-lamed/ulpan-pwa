{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Foundation where

import BasicPrelude
import Text.Lucius       (luciusFile)
import Text.Hamlet       (hamletFile)
import Yesod.Core
import Yesod.Static      (publicFiles, Static)
import qualified Model.Vocab       as V
import qualified Data.Map.Strict as M

-- not using `staticFiles` since we're not using etags - service-worker is responsible for caching
-- However, putting etags back in may be a more robust solution (See https://github.com/GoogleChrome/sw-precache)
-- (but this doesn't cover internal bootstrap reference to fonts)
publicFiles "static"

data App = App
  { getStatic      :: Static
  , aLanguageFiles :: M.Map Text V.LanguageFile
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
  makeSessionBackend _ = return Nothing
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent $ do
      contents
      addStylesheet $ StaticR css_bootstrap_min_css
      addStylesheet $ StaticR css_bootstrap_theme_min_css
      addScript $ StaticR js_jquery_2_1_4_min_js
      addScript $ StaticR js_bootstrap_min_js
      addScript $ StaticR js_menu_js
      toWidget $(luciusFile "templates/main.lucius")
    withUrlRenderer $(hamletFile "templates/main.hamlet")
