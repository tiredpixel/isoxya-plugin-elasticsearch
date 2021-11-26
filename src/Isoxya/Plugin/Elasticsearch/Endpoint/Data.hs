module Isoxya.Plugin.Elasticsearch.Endpoint.Data (
    create,
    ) where


import           Control.Lens
import           Data.Aeson.Lens
import           Data.Scientific                        (scientific)
import           Data.Time.Clock
import           Isoxya.Plugin.Elasticsearch.Core       hiding (formatTime)
import           Network.URI
import           TiredPixel.Common.Isoxya.Snap.Streamer ()
import           TiredPixel.Common.Isoxya.Streamer
import           TiredPixel.Common.URI
import qualified Crypto.Hash                            as Hash
import qualified Data.ByteString.Lazy.Char8             as C8
import qualified Data.Time.Format                       as Time
import qualified Data.Vector                            as V
import qualified Network.HTTP.Conduit                   as HTTP
import qualified Network.HTTP.Types.Status              as HTTP
import qualified TiredPixel.Common.Net                  as N


create :: Handler b Elasticsearch ()
create = do
    u <- gets _up
    n <- gets _net
    let reqURL = relativeTo uPath u
    req_      <- getBoundedJSON' reqLim >>= validateJSON
    Just strm <- runValidate req_
    let strms' = convStrm strm
    let resultsN = toInteger $ length strms'
    let uBody = C8.unlines $ concat [[
            encode $ jAction strm i,
            encode $ jDataNS $ mergeObject (toJSON strm') $ jDataMeta i resultsN
            ] | (i, strm') <- zip [1..] strms']
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


convStrm :: Streamer -> [Streamer]
convStrm strm = if null r then rDef else r
    where
        datSpellchecker = [mergeObject result $ object [
                ("paragraph", String $ datum ^. key "paragraph" . _String)] |
            datum  <- V.toList $ streamerData strm ^. _Array,
            result <- V.toList $ datum ^. key "results" . _Array]
        r = case streamerProcessorTag strm of
            "spellchecker" -> [strm {
                streamerData = datum} | datum <- datSpellchecker]
            _ -> rDef
        rDef = [strm]

dId :: Streamer -> Integer -> Text
dId strm i = show _idh <> "." <> show i
    where
        _idh = hash (streamerCrawlHref strm <> "|" <>
            show (unURIAbsolute $ streamerURL strm) <> "|" <>
            streamerProcessorHref strm)

dIndex :: Streamer -> Maybe Text
dIndex strm = do
    let time = streamerCrawlBegan strm
    return $ _ns <> _sep <> formatTime time
    where
        _sep = "."
        _ns = "isoxya" :: Text

formatTime :: UTCTime -> Text
formatTime = toText . Time.formatTime Time.defaultTimeLocale "%F"

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)

jAction :: Streamer -> Integer -> Maybe Value
jAction strm i = do
    dIndex' <- dIndex strm
    return $ object [
        ("index", object [
            ("_index", String dIndex'),
            ("_id", String $ dId strm i)])]

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
