module ISX.Pipe.Elasticsearch.Zone.Common.Data (
    create
    ) where


import              Control.Lens
import              Data.Aeson
import              Data.Aeson.Lens
import              Data.Scientific                         (scientific)
import              PVK.Com.API.Aeson
import              PVK.Com.API.Resource.ISXPipeSnap        ()
import              Snap.Core
import              System.Environment                      (lookupEnv)
import qualified    Crypto.Hash                             as  Hash
import qualified    Data.ByteString.Lazy.Char8              as  C8
import qualified    Data.Text                               as  T
import qualified    Data.Vector                             as  V
import qualified    Network.HTTP.Conduit                    as  HTTP
import qualified    Network.HTTP.Types.Status               as  HTTPTS
import qualified    Network.URI                             as  URI
import qualified    PVK.Com.API.Ext.URI                     as  URI
import qualified    PVK.Com.API.Req                         as  Req
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.API.Resource.ISXPipe            as  R
import qualified    PVK.Com.Net                             as  Net


create :: URI.URI -> Net.Conn -> Snap ()
create dUrl n = do
    let Just dPath = URI.parseRelativeReference "/_bulk"
    let reqUrl = URI.relativeTo dPath dUrl
    reqLim_ <- liftIO $ join <$> (fmap . fmap) readMaybe (lookupEnv "REQ_LIM")
    let reqLim = fromMaybe reqLimDef reqLim_
    req_      <- Req.getBoundedJSON' reqLim >>= Req.validateJSON
    Just drpl <- Res.runValidate req_
    let drpls' = convDroplet drpl
    let results_n = toInteger $ length drpls'
    let uBody = C8.unlines $ concat [[
            encode $ jAction drpl i,
            encode $ mergeObject (toJSON drpl') $ jDataMeta i results_n
            ] | (i, drpl') <- zip [1..] drpls']
    let uReq = Net.jsonNDReq $ Net.makeReq "POST" reqUrl uBody
    uRes <- liftIO $ Net.makeRes uReq n
    modifyResponse $ setResponseCode $
        HTTPTS.statusCode $ HTTP.responseStatus uRes
    writeLBS $ HTTP.responseBody uRes


convDroplet :: R.Droplet -> [R.Droplet]
convDroplet drpl = if null r then rDef else r
    where
        dataSpellchecker = [mergeObject result $ object [
                ("paragraph", String $ datum ^. key "paragraph" . _String)] |
            datum  <- V.toList $ R.dropletData drpl ^. _Array,
            result <- V.toList $ datum ^. key "results" . _Array]
        r = case R.dropletOrgPickTag drpl of
            "spellchecker" -> [drpl {
                R.dropletData = datum} | datum <- dataSpellchecker]
            _ -> rDef
        rDef = [drpl]

dId :: R.Droplet -> Integer -> Text
dId drpl i = show _idh <> "." <> show i
    where
        _idh = hash (show (URI.unURIAbsolute $ R.dropletUrl drpl) <> "|" <>
            R.dropletOrgPickHref drpl)

dIndex :: R.Droplet -> Maybe Text
dIndex drpl = do
    (site, snap) <- unSiteSnapHref $ R.dropletSiteSnapHref drpl
    let site' = show $ hash site
    let snap' = T.toLower $ T.replace ":" "-" snap
    return $ _index_pre <> site' <> "-" <> snap'
    where
        _index_pre = "isoxya-" :: Text

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)

jAction :: R.Droplet -> Integer -> Maybe Value
jAction drpl i = do
    dIndex' <- dIndex drpl
    return $ object [
        ("index", object [
            ("_index", String dIndex'),
            ("_id", String $ dId drpl i)])]

jDataMeta :: Integer -> Integer -> Value
jDataMeta i n = object [
    ("data_i", Number $ scientific i 0),
    ("data_n", Number $ scientific n 0)]

reqLimDef :: Int64
reqLimDef = 2097152 -- 2 MB = (1 + .5) * (4/3) MB

unSiteSnapHref :: Text -> Maybe (Text, Text)
unSiteSnapHref h = do
    ["", "site", s, "site_snap", n] <- return $ T.splitOn "/" h
    return (s, n)
