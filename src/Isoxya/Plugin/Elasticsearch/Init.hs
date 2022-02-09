module Isoxya.Plugin.Elasticsearch.Init (
    initElasticsearch,
    routesElasticsearch,
    ) where


import qualified Isoxya.Plugin.Elasticsearch.Endpoint.Apex as Apx
import qualified Isoxya.Plugin.Elasticsearch.Endpoint.Data as Dat
import           Isoxya.Plugin.Elasticsearch.Type
import           Network.URI
import           Snap.Core
import           Snap.Snaplet
import qualified TiredPixel.Common.Net                     as N
import           TiredPixel.Common.Snap.CoreUtil


initElasticsearch :: URI -> N.Conn -> SnapletInit b Elasticsearch
initElasticsearch u n = makeSnaplet "Elasticsearch" "" Nothing $ do
    addRoutes routesElasticsearch
    return $ Elasticsearch u n

routesElasticsearch :: [(ByteString, Handler b Elasticsearch ())]
routesElasticsearch = [
    ("",        ifTop       Apx.apex),
    --
    ("data",    method POST Dat.create),
    ("data/:_",             notFound),
    --
    ("",                    notFound)]
