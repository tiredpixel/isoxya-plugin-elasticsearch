module ISX.Pipe.Elasticsearch.Zone.Common.Data (
    create
    ) where


import              Data.Aeson
import              PVK.Com.API.Resource.ISXPipeSnap        ()
import              Snap.Core
import qualified    Crypto.Hash                             as  Hash
import qualified    Data.Text                               as  T
import qualified    Network.HTTP.Conduit                    as  HTTP
import qualified    Network.HTTP.Types.Status               as  HTTPTS
import qualified    Network.URI                             as  URI
import qualified    PVK.Com.API.Req                         as  Req
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.API.Resource.ISXPipe            as  R
import qualified    PVK.Com.Net                             as  Net


create :: URI.URI -> Net.Conn -> Snap ()
create dUrl n = do
    req_      <- Req.getBoundedJSON' s >>= Req.validateJSON
    Just drpl <- Res.runValidate req_
    let Just reqUrl = dEndpoint dUrl drpl
    let uReq = Net.jsonReq $ Net.makeReq' "POST" reqUrl $ encode drpl
    uRes <- liftIO $ Net.makeRes uReq n
    modifyResponse $ setResponseCode $
        HTTPTS.statusCode $ HTTP.responseStatus uRes
    where
        s = 50000000 -- 50 MB


dEndpoint :: URI.URI -> R.Droplet -> Maybe URI.URI
dEndpoint dUrl drpl = do
    (site, snap) <- unSiteSnapHref $ R.dropletSiteSnapHref drpl
    let site' = show $ hash site
    let snap' = T.toLower $ T.replace ":" "-" snap
    let _index = "/" <> _index_pre <> site' <> "-" <> snap'
    dPath <- URI.parseRelativeReference . toString $
        _index <> "/" <> _type <> "/"
    return $ URI.relativeTo dPath dUrl
    where
        _index_pre = "isoxya-" :: Text
        _type = "_doc" :: Text
        --_id = -- TODO: wait for #952; hash(url, org_pick.href, data[#]?)

hash :: Text -> Hash.Digest Hash.SHA256
hash t = Hash.hash (encodeUtf8 t :: ByteString)

unSiteSnapHref :: Text -> Maybe (Text, Text)
unSiteSnapHref h = do
    ["", "site", s, "site_snap", n] <- return $ T.splitOn "/" h
    return (s, n)
