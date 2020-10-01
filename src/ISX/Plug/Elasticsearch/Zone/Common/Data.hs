module ISX.Plug.Elasticsearch.Zone.Common.Data (
    create
    ) where


import              Control.Lens
import              Data.Aeson
import              Data.Aeson.Lens
import              Data.Scientific                         (scientific)
import              Data.Time.Clock                         (UTCTime)
import              Snap.Core
import              System.Environment                      (lookupEnv)
import              TPX.Com.API.Aeson
import              TPX.Com.API.Resource.ISX.StrmSnap       ()
import qualified    Crypto.Hash                             as  Hash
import qualified    Data.ByteString.Lazy.Char8              as  C8
import qualified    Data.Text                               as  T
import qualified    Data.Time.Format                        as  Time
import qualified    Data.Vector                             as  V
import qualified    Network.HTTP.Conduit                    as  HTTP
import qualified    Network.HTTP.Types.Status               as  HTTPTS
import qualified    Network.URI                             as  URI
import qualified    TPX.Com.API.Ext.URI                     as  URI
import qualified    TPX.Com.API.Req                         as  Req
import qualified    TPX.Com.API.Res                         as  Res
import qualified    TPX.Com.API.Resource.ISX.Strm           as  R
import qualified    TPX.Com.Net                             as  Net


create :: URI.URI -> Net.Conn -> Snap ()
create dUrl n = do
    let Just dPath = URI.parseRelativeReference "/_bulk"
    let reqUrl = URI.relativeTo dPath dUrl
    reqLim_ <- liftIO $ join <$> (fmap . fmap) readMaybe (lookupEnv "REQ_LIM")
    let reqLim = fromMaybe reqLimDef reqLim_
    req_      <- Req.getBoundedJSON' reqLim >>= Req.validateJSON
    Just strm <- Res.runValidate req_
    let strms' = convStrm strm
    let results_n = toInteger $ length strms'
    let uBody = C8.unlines $ concat [[
            encode $ jAction strm i,
            encode $ mergeObject (toJSON strm') $ jDataMeta i results_n
            ] | (i, strm') <- zip [1..] strms']
    let uReq = Net.jsonNDReq $ Net.makeReq "POST" reqUrl uBody
    uRes <- liftIO $ Net.makeRes uReq n
    modifyResponse $ setResponseCode $
        HTTPTS.statusCode $ HTTP.responseStatus uRes
    writeLBS $ HTTP.responseBody uRes


convStrm :: R.Strm -> [R.Strm]
convStrm strm = if null r then rDef else r
    where
        dataSpellchecker = [mergeObject result $ object [
                ("paragraph", String $ datum ^. key "paragraph" . _String)] |
            datum  <- V.toList $ R.strmData strm ^. _Array,
            result <- V.toList $ datum ^. key "results" . _Array]
        r = case R.strmOrgProcTag strm of
            "spellchecker" -> [strm {
                R.strmData = datum} | datum <- dataSpellchecker]
            _ -> rDef
        rDef = [strm]

dId :: R.Strm -> Integer -> Text
dId strm i = show _idh <> "." <> show i
    where
        _idh = hash (R.strmSiteSnapHref strm <> "|" <>
            show (URI.unURIAbsolute $ R.strmUrl strm) <> "|" <>
            R.strmOrgProcHref strm)

dIndex :: R.Strm -> Maybe Text
dIndex strm = do
    org <- unOrgHref $ R.strmOrgHref strm
    let time = R.strmSiteSnapTBegin strm
    return $ _ns <> _sep <> org <> _sep <> formatTime time
    where
        _sep = "."
        _ns = "isoxya" :: Text

formatTime :: UTCTime -> Text
formatTime = toText . Time.formatTime Time.defaultTimeLocale "%F"

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)

jAction :: R.Strm -> Integer -> Maybe Value
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

reqLimDef :: Int64
reqLimDef = 2097152 -- 2 MB = (1 + .5) * (4/3) MB

unOrgHref :: Text -> Maybe Text
unOrgHref h = do
    ["", "org", o] <- return $ T.splitOn "/" h
    return o
