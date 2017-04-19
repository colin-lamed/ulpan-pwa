{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
 ( makeApplication
 ) where

import BasicPrelude
import Data.FileEmbed (embedFile)
import Foundation
import Yesod.Core
import Yesod.Default.Config (AppConfig, DefaultEnv)
import Yesod.Static (static)
import Handler.Home
import Model.VocabApi (loadVocab)
import qualified Data.Map.Strict as M

mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv extra -> IO Application
makeApplication _ = do
  s <- static "static"
  let vocab = [ ("ulpan",                    loadVocab $(embedFile "resources/vocab/ulpan.yml"))
              , ("hebrew_colloquial_course", loadVocab $(embedFile "resources/vocab/hebrew_colloquial_course.yml"))
              , ("hsk1",                     loadVocab $(embedFile "resources/vocab/hsk1.yml"))
              ]
  toWaiAppPlain $ App
             { getStatic      = s
             , aLanguageFiles = M.fromList vocab
             }
