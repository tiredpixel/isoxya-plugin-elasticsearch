module ISX.Plug.Elasticsearch.Route (site) where


import              Network.URI                             (URI)
import              Snap.Core
import qualified    ISX.Plug.Elasticsearch.Zone.Common.Apex as  ZA
import qualified    ISX.Plug.Elasticsearch.Zone.Common.Data as  ZD
import qualified    TPX.Com.API.Res                         as  Res
import qualified    TPX.Com.Net                             as  Net


site :: URI -> Net.Conn -> Snap ()
site dUrl n = ifTop ZA.apex <|> route [
    -- COMMON
    ("data",                            method POST $   ZD.create dUrl n),
    ("data/:_",                                         notFound),
    --
    ("",                                                notFound)]
    where
        notFound = Res.notFound
