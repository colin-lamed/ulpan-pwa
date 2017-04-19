{-# LANGUAGE FlexibleInstances #-}

module Model.VocabSerialisation where

import Prelude
import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, genericParseJSON, genericToEncoding)
import Model.Vocab
import qualified Data.Map as M


instance (ToJSON v) => ToJSON (M.Map Group v) where
    toJSON = toJSON . M.mapKeys unGroup

instance (FromJSON v) => FromJSON (M.Map Group v) where
    parseJSON v = parseJSON v >>= return . M.mapKeys Group

instance ToJSON LanguageFile where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LanguageFile where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Meta where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Meta where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Group where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Group where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Language where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Language where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON VocabEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON VocabEntry where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON TextSize where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TextSize where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Gravity where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Gravity where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON InitData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON InitData where
  parseJSON = genericParseJSON defaultOptions
