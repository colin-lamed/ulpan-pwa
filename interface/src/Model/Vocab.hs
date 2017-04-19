{-# LANGUAGE DeriveGeneric     #-}

module Model.Vocab
 ( Language(..)
 , VocabEntry(..)
 , LanguageFile(..)
 , Meta(..)
 , Gravity(..)
 , TextSize(..)
 , Group(..)
 , InitData(..)
 ) where

import Prelude
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Map as M

newtype Language = Language { lText :: Text }
  deriving (Read, Show, Generic)

data VocabEntry = VocabEntry Language Language Text
  deriving (Read, Show, Generic)

data LanguageFile =
  LanguageFile { lfMeta  :: Meta
               , lfVocab :: M.Map Group [VocabEntry]
               }
  deriving (Show, Generic)

data Meta = Meta { name              :: Text
                 , language1Key      :: Text
                 , language2Key      :: Text
                 , noteKey           :: Text
                 , language1Gravity  :: Gravity
                 , language2Gravity  :: Gravity
                 , language1TextSize :: TextSize
                 , language2TextSize :: TextSize
                 } deriving (Show, Generic)

data Gravity = GCentre
             | GRight
             | GLeft
  deriving (Show, Generic)

data TextSize = Small
              | Large
              | XLarge
              | Medium
  deriving (Show, Generic)

newtype Group = Group { unGroup :: Text } deriving (Read, Show, Eq, Ord, Generic)



data InitData = InitData { languagesAvailable :: [(Text, Text)]
                         } deriving (Show, Generic)
