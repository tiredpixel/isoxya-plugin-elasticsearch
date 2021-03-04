module ISX.Plug.Elasticsearch.Init (
    initElasticsearch,
    routesElasticsearch,
    ) where


import           ISX.Plug.Elasticsearch.Types
import           Network.URI
import           Snap.Core
import           Snap.Snaplet
import           TPX.Com.Snap.CoreUtils
import qualified ISX.Plug.Elasticsearch.Zone.Apex as ZA
import qualified ISX.Plug.Elasticsearch.Zone.Data as ZD
import qualified TPX.Com.Net                      as N


initElasticsearch :: URI -> N.Conn -> SnapletInit b Elasticsearch
initElasticsearch u n = makeSnaplet "Elasticsearch" "" Nothing $ do
    addRoutes routesElasticsearch
    return $ Elasticsearch u n

routesElasticsearch :: [(ByteString, Handler b Elasticsearch ())]
routesElasticsearch = [
    ("",                                    ifTop           ZA.apex),
    --
    ("data",                                method POST     ZD.create),
    ("data/:_",                                             notFound),
    --
    ("",                                                    notFound)]
