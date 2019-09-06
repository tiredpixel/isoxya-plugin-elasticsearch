module ISX.Pipe.Elasticsearch.Route (site) where


import              Network.URI                             (URI)
import              Snap.Core
import qualified    ISX.Pipe.Elasticsearch.Zone.Common.Apex as  ZA
import qualified    ISX.Pipe.Elasticsearch.Zone.Common.Data as  ZD
import qualified    Network.HTTP.Conduit                    as  Net
import qualified    PVK.Com.API.Zone.Common.Error           as  ZE


site :: URI -> Net.Manager -> Snap ()
site _ _ = ifTop ZA.apex <|> route [
    -- COMMON
    ("data",                            method POST     ZD.create),
    ("data/:_",                                         notFound),
    --
    ("",                                                notFound)]
    where
        notFound = ZE.notFound
