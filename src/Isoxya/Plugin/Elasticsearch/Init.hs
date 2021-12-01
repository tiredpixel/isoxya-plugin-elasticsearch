module Isoxya.Plugin.Elasticsearch.Init (
    initElasticsearch,
    routesElasticsearch,
    ) where


import           Isoxya.Plugin.Elasticsearch.Type
import           Network.URI
import           Snap.Core
import           Snap.Snaplet
import           TiredPixel.Common.Snap.CoreUtil
import qualified Isoxya.Plugin.Elasticsearch.Endpoint.Apex as EA
import qualified Isoxya.Plugin.Elasticsearch.Endpoint.Data as ED
import qualified TiredPixel.Common.Net                     as N


initElasticsearch :: URI -> N.Conn -> SnapletInit b Elasticsearch
initElasticsearch u n = makeSnaplet "Elasticsearch" "" Nothing $ do
    addRoutes routesElasticsearch
    return $ Elasticsearch u n

routesElasticsearch :: [(ByteString, Handler b Elasticsearch ())]
routesElasticsearch = [
    ("",        ifTop       EA.apex),
    --
    ("data",    method POST ED.create),
    ("data/:_",             notFound),
    --
    ("",                    notFound)]
