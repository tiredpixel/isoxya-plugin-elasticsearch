module ISX.Plugin.Elasticsearch.Route (site) where


import              Network.URI                             (URI)
import              Snap.Core
import qualified    ISX.Plugin.Elasticsearch.Zone.Common.Apex as  ZA
import qualified    ISX.Plugin.Elasticsearch.Zone.Common.Data as  ZD
import qualified    PVK.Com.API.Zone.Common.Error           as  ZE
import qualified    PVK.Com.Net                             as  Net


site :: URI -> Net.Conn -> Snap ()
site dUrl n = ifTop ZA.apex <|> route [
    -- COMMON
    ("data",                            method POST $   ZD.create dUrl n),
    ("data/:_",                                         notFound),
    --
    ("",                                                notFound)]
    where
        notFound = ZE.notFound
