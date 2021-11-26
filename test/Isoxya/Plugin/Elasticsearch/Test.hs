module Isoxya.Plugin.Elasticsearch.Test (
    module Isoxya.Plugin.Elasticsearch,
    module Isoxya.Plugin.Elasticsearch.Core,
    module TiredPixel.Common.Snap.Test,
    fixtureResult,
    genStreamer,
    snapElasticsearch,
    ) where


import           Data.Time.Clock                   (UTCTime)
import           Isoxya.Plugin.Elasticsearch
import           Isoxya.Plugin.Elasticsearch.Core  hiding (addHeader, setContentType, setHeader, (.=))
import           Network.URI
import           System.IO                         hiding (print)
import           TiredPixel.Common.Isoxya.Streamer
import           TiredPixel.Common.Snap.Test
import           TiredPixel.Common.URI
import qualified Data.Text                         as T
import qualified Data.Time.Format                  as Time
import qualified TiredPixel.Common.Net             as N


fixtureResult :: Text -> Text -> FilePath
fixtureResult ns url = toString $ "test/fixture/results/" <> ns <> "/" <>
    fxExt url <> ".ndjson"

genStreamer :: Text -> Text -> Value -> Streamer
genStreamer url tag dat = Streamer {
    streamerCrawlBegan    = began,
    streamerCrawlHref     = "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crawl/2019-05-01T06:19:54.48295Z",
    streamerData          = dat,
    streamerProcessorHref = "/processor/8ddf53cc-6a72-11e9-8001-0242ac160005",
    streamerProcessorTag  = tag,
    streamerRetrieved     = retrieved,
    streamerSiteHref      = "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
    streamerSiteURL       = siteURL,
    streamerURL           = strmURL}
    where
        Just began = parseTime "2019-05-01T06:19:54.48295Z"
        Just retrieved = parseTime "2019-05-01T06:06:48.740524Z"
        Just siteURL = URIAbsolute <$>
            parseAbsoluteURI (toString $ "http://" <> url)
        Just strmURL = URIAbsolute <$>
            parseAbsoluteURI (toString $ "http://" <> url <> "/" <> tag)

snapElasticsearch :: SpecWith (SnapHspecState Elasticsearch) -> Spec
snapElasticsearch = snap (route routesElasticsearch) initElasticsearchTest


fxExt :: Text -> Text
fxExt url = if T.takeEnd 1 url == "/"
    then url <> "index.html"
    else url

initElasticsearchTest :: SnapletInit b Elasticsearch
initElasticsearchTest = makeSnaplet "API" "" Nothing $ do
    uE <- liftIO $ lookupEnv "ELASTICSEARCH_HOST"
    let Just u = parseAbsoluteURI $ fromMaybe uDef uE
    n <- liftIO N.openConn
    addRoutes routesElasticsearch
    return $ Elasticsearch u n
    where
        uDef = "http://test_echo"

parseTime :: String -> Maybe UTCTime
parseTime = Time.parseTimeM False Time.defaultTimeLocale timeF

timeF :: String
timeF = Time.iso8601DateFormat (Just "%T%QZ")
