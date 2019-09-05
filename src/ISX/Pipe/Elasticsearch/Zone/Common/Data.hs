module ISX.Pipe.Elasticsearch.Zone.Common.Data (
    create
    ) where


import              PVK.Com.API.Resource.ISXPipeSnap        ()
import              Snap.Core
import qualified    PVK.Com.API.Req                         as  Req
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.API.Resource.ISXPipe            as  R


create :: Snap ()
create = do
    req_   <- Req.getBoundedJSON' s >>= Req.validateJSON
    Just _ <- Res.runValidate req_ :: Snap (Maybe R.Droplet)
    --writeJSON $ R.Ore {
    --    R.oreData = toJSON $ R.DataStatus {
    --        R.dataStatusCode = R.rockMetaStatusCode $ R.rockMeta rock},
    --    R.oreUrls = S.empty}
    Res.notFound -- TODO
    where
        s = 50000000 -- 50 MB
