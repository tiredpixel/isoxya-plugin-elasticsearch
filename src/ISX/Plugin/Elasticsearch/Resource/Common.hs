module ISX.Plugin.Elasticsearch.Resource.Common (
    Apex(..)
    ) where


import              Data.Aeson
import              Data.Time.Clock                         (UTCTime)


data Apex = Apex {
    apexTNow    :: UTCTime,
    apexVersion :: Text
    } deriving (Show)
instance ToJSON Apex where
    toJSON o = object [
        "t_now"   .= apexTNow o,
        "version" .= apexVersion o]
