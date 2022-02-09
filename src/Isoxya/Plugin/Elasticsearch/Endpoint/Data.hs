module Isoxya.Plugin.Elasticsearch.Endpoint.Data (
    create,
    ) where


import           Control.Lens
import qualified Crypto.Hash                            as Hash
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8             as C8
import           Data.Scientific                        (scientific)
import           Data.Time.Clock
import qualified Data.Time.Format                       as Time
import qualified Data.Vector                            as V
import           Isoxya.Plugin.Elasticsearch.Core       hiding (formatTime)
import qualified Network.HTTP.Conduit                   as HTTP
import qualified Network.HTTP.Types.Status              as HTTP
import           Network.URI
import           TiredPixel.Common.Isoxya.Snap.Streamer ()
import           TiredPixel.Common.Isoxya.Streamer
import qualified TiredPixel.Common.Net                  as N
import           TiredPixel.Common.URI


create :: Handler b Elasticsearch ()
create = do
    u <- gets _up
    n <- gets _net
    let reqURL = relativeTo uPath u
    req_ <- getBoundedJSON' reqLim >>= validateJSON
    Just str <- runValidate req_
    let strs' = convertStreamer str
    let resultsN = toInteger $ length strs'
    let uBody = C8.unlines $ concat [[
            encode $ jAction str i,
            encode $ jDataNS $ mergeObject (toJSON str') $ jDataMeta i resultsN
            ] | (i, str') <- zip [1..] strs']
    let uReq = N.jsonNDReq $ N.makeReq "POST" reqURL uBody
    uRes <- liftIO $ N.makeRes uReq n
    let rx_ = decode $ HTTP.responseBody uRes :: Maybe ESBulkRes
    modifyResponse $ setResponseCode $ case esBulkResErrors <$> rx_ of
        Just True -> 400
        _         -> HTTP.statusCode $ HTTP.responseStatus uRes
    writeLBS $ HTTP.responseBody uRes
    where
        Just uPath = parseRelativeReference "/_bulk"
        reqLim = 2097152 -- 2 MB = (1 + .5) * (4/3) MB


convertStreamer :: Streamer -> [Streamer]
convertStreamer str = if null r then rDef else r
    where
        datSpellchecker = [mergeObject result $ object [
                ("paragraph", String $ datum ^. key "paragraph" . _String)] |
            datum  <- V.toList $ streamerData str ^. _Array,
            result <- V.toList $ datum ^. key "results" . _Array]
        r = case streamerProcessorTag str of
            "spellchecker" -> [str {
                streamerData = datum} | datum <- datSpellchecker]
            _ -> rDef
        rDef = [str]

dId :: Streamer -> Integer -> Text
dId str i = show _idh <> "." <> show i
    where
        _idh = hash (streamerCrawlHref str <> "|" <>
            show (unURIAbsolute $ streamerURL str) <> "|" <>
            streamerProcessorHref str)

dIndex :: Streamer -> Maybe Text
dIndex str = do
    let time = streamerCrawlBegan str
    return $ _ns <> _sep <> formatTime time
    where
        _sep = "."
        _ns = "isoxya" :: Text

formatTime :: UTCTime -> Text
formatTime = toText . Time.formatTime Time.defaultTimeLocale "%F"

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)

jAction :: Streamer -> Integer -> Maybe Value
jAction str i = do
    dIndex' <- dIndex str
    return $ object [
        ("index", object [
            ("_index", String dIndex'),
            ("_id", String $ dId str i)])]

jDataMeta :: Integer -> Integer -> Value
jDataMeta i n = object [
    ("data_i", Number $ scientific i 0),
    ("data_n", Number $ scientific n 0)]

-- ES throws errors when importing keys with types different to those already
-- auto-detected. Thus, we append the Processor tag as a suffix to the key. The
-- way of doing this here is hacky, and there is surely a better way -- perhaps
-- using lenses better, or maybe avoiding this whole approach and instead
-- handling it at the ToJSON instance level.
jDataNS :: Value -> Value
jDataNS j = mergeObject j $ object [
    ("data." <> j ^. key "processor" . key "tag" . _String,
        Object $ j ^. key "data" . _Object),
    ("data", Null)]
