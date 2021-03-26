module ISX.Plug.Elasticsearch.Test (
    module ISX.Plug.Elasticsearch,
    module ISX.Plug.Elasticsearch.Core,
    module TPX.Com.Snap.Test,
    fixtureResult,
    genPlugStrm,
    snapElasticsearch,
    ) where


import           Data.Time.Clock             (UTCTime)
import           ISX.Plug.Elasticsearch
import           ISX.Plug.Elasticsearch.Core hiding (addHeader, setContentType, setHeader, (.=))
import           Network.URI
import           System.IO                   hiding (print)
import           TPX.Com.Isoxya.PlugStrm
import           TPX.Com.Snap.Test
import           TPX.Com.URI
import qualified Data.Text                   as T
import qualified Data.Time.Format            as Time
import qualified TPX.Com.Net                 as N


fixtureResult :: Text -> Text -> FilePath
fixtureResult ns url = toString $ "test/fixture/results/" <> ns <> "/" <>
    fxExt url <> ".ndjson"

genPlugStrm :: Text -> Text -> Value -> PlugStrm
genPlugStrm url tag dat = PlugStrm {
    plugStrmCrwlHref     = "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z",
    plugStrmCrwlTBegin   = tBegin,
    plugStrmSiteHref     = "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw",
    plugStrmSiteURL      = siteURL,
    plugStrmOrgHref      = "/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd",
    plugStrmPlugProcHref = "/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005",
    plugStrmPlugProcTag  = tag,
    plugStrmURL          = strmURL,
    plugStrmTRetrieval   = tRetrieval,
    plugStrmData         = dat}
    where
        Just tBegin = parseTime "2019-05-01T06:19:54.48295Z"
        Just tRetrieval = parseTime "2019-05-01T06:06:48.740524Z"
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
