{-# LANGUAGE OverloadedStrings #-}

module Model.VocabApi
 ( loadVocab
 ) where

import BasicPrelude
import Data.Yaml (FromJSON, (.:), Value(Array, Object, String), Parser, parseJSON)
import Model.Vocab
import qualified Data.Map as M
import qualified Data.HashMap.Strict as M2
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y

parseVocabEntries :: Meta -> Value -> [VocabEntry]
parseVocabEntries m (Array vec) = catMaybes $ fmap (Y.parseMaybe $ parseVocabEntry m) (zip [1..] (V.toList vec))
parseVocabEntries _ e           = error $ T.unpack $ "Unexpected type for [VocabEntry], was " <> show e

parseVocabEntry :: Meta -> (Int, Value) -> Parser VocabEntry
parseVocabEntry m (_, (Object o)) = VocabEntry <$>
                                      o .: language1Key m <*>
                                      o .: language2Key m <*>
                                      (o .: noteKey m <|> pure "")
parseVocabEntry _ (i, e)          = error $ T.unpack $ "Could not parse entry " <> show i <> ": " <> show e

loadVocab :: ByteString -> LanguageFile
loadVocab ymlData =
  let maybeResults = Y.decode ymlData :: Maybe Y.Object
      results = fromMaybe (error "Could not read yml") maybeResults
      maybeMeta = M2.lookup "meta" results >>= Y.parseMaybe parseJSON
      meta = fromMaybe (error "could not parse meta") maybeMeta
      vocab = let d = M2.filterWithKey (\k _ -> k /= "meta") results
              in M2.foldrWithKey (\k v a -> M.insert (Group k) (parseVocabEntries meta v) a) M.empty d
  in LanguageFile { lfMeta = meta, lfVocab = vocab }


instance FromJSON Gravity where
    parseJSON (String "right")  = return GRight
    parseJSON (String "centre") = return GCentre
    parseJSON _                 = return GLeft

instance FromJSON TextSize where
    parseJSON (String "small")  = return Small
    parseJSON (String "large")  = return Large
    parseJSON (String "xlarge") = return XLarge
    parseJSON _                 = return Medium

instance FromJSON Language where
    parseJSON (String l)  = return $ Language l
    parseJSON e           = error $ T.unpack $ "Can't parse Language " <> show e


instance FromJSON Meta where
    parseJSON (Object o) = Meta <$>
                           o .: "name" <*>
                           o .: "language1" <*>
                           o .: "language2" <*>
                           o .: "note" <*>
                           (o .: "language1_gravity" <|> pure GLeft) <*>
                           (o .: "language2_gravity" <|> pure GLeft) <*>
                           (o .: "language1_textsize" <|> pure Medium) <*>
                           (o .: "language2_textsize" <|> pure Medium)
    parseJSON _          = error "Can't parse Meta from YAML/JSON"
