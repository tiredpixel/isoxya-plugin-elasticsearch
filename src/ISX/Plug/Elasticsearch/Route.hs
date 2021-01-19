module ISX.Plug.Elasticsearch.Route (site) where


import              Network.URI
import              Snap.Core
import              TPX.Com.API.Res
import qualified    ISX.Plug.Elasticsearch.Zone.Apex        as  ZA
import qualified    ISX.Plug.Elasticsearch.Zone.Data        as  ZD
import qualified    TPX.Com.Net                             as  N


site :: URI -> N.Conn -> Snap ()
site u n = ifTop ZA.apex <|> route [
    ("data",                                method POST $   ZD.create u n),
    ("data/:_",                                             notFound),
    --
    ("",                                                    notFound)]
