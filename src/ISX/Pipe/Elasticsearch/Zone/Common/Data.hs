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
import qualified    Crypto.Hash                             as  Hash
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
    req_      <- Req.getBoundedJSON' s >>= Req.validateJSON
    Just drpl <- Res.runValidate req_
    let drpls' = convDroplet drpl
    let results_n = toInteger $ length drpls'
    for_ (zip [1..] drpls') $ \(i, drpl') -> do
        let Just reqUrl = dEndpoint dUrl drpl i
        let uJson = mergeObject (toJSON drpl') $ object [
                ("data_i", Number $ scientific i 0),
                ("data_n", Number $ scientific results_n 0)]
        let uBody = encode uJson <> "\n" -- newline to make testing easier
        let uReq = Net.jsonReq $ Net.makeReq "PUT" reqUrl uBody
        uRes <- liftIO $ Net.makeRes uReq n
        modifyResponse $ setResponseCode $
            HTTPTS.statusCode $ HTTP.responseStatus uRes
        writeLBS $ HTTP.responseBody uRes
    where
        s = 50000000 -- 50 MB


convDroplet :: R.Droplet -> [R.Droplet]
convDroplet drpl = case R.dropletOrgPickTag drpl of
    "spellchecker" -> [drpl {
        R.dropletData = datum} | datum <- dataSpellchecker]
    _ -> [drpl]
    where
        dataSpellchecker = [mergeObject result $ object [
                ("paragraph", String $ datum ^. key "paragraph" . _String)] |
            datum  <- V.toList $ R.dropletData drpl ^. _Array,
            result <- V.toList $ datum ^. key "results" . _Array]

dEndpoint :: URI.URI -> R.Droplet -> Integer -> Maybe URI.URI
dEndpoint dUrl drpl i = do
    (site, snap) <- unSiteSnapHref $ R.dropletSiteSnapHref drpl
    let site' = show $ hash site
    let snap' = T.toLower $ T.replace ":" "-" snap
    let _index = "/" <> _index_pre <> site' <> "-" <> snap'
    dPath <- URI.parseRelativeReference . toString $
        _index <> "/" <> _type <> "/" <> _id
    return $ URI.relativeTo dPath dUrl
    where
        _index_pre = "isoxya-" :: Text
        _type = "_doc" :: Text
        _idh = hash (show (URI.unURIAbsolute $ R.dropletUrl drpl) <> "|" <>
            R.dropletOrgPickHref drpl)
        _id = show _idh <> "." <> show i

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)

unSiteSnapHref :: Text -> Maybe (Text, Text)
unSiteSnapHref h = do
    ["", "site", s, "site_snap", n] <- return $ T.splitOn "/" h
    return (s, n)
