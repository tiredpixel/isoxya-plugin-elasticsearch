{-# LANGUAGE RecordWildCards #-}


module Isoxya.Plugin.Elasticsearch.Resource (
    Apex(..),
    ESBulkRes(..),
    ) where


import Data.Aeson
import Data.Time.Clock


data Apex = Apex {
    apexNow     :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON Apex{..} = object [
        "now"     .= apexNow,
        "version" .= apexVersion]

newtype ESBulkRes = ESBulkRes {
    esBulkResErrors :: Bool
    } deriving (Show)
instance FromJSON ESBulkRes where
    parseJSON = withObject "es.bulk.res" $ \j -> do
        esBulkResErrors <- j .: "errors"
        return ESBulkRes{..}
