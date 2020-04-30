module ISX.Test (
    module PVK.Com.API.Test,
    withSrv
    ) where


import              ISX.Plugin.Elasticsearch.Route
import              Network.URI
import              PVK.Com.API.Test
import              System.Environment                      (getEnv)
import qualified    PVK.Com.Net                             as  Net


withSrv :: RequestBuilder IO () -> IO Response
withSrv r = do
    Just dUrl <- parseAbsoluteURI <$> getEnv "_TEST_ELASTICSEARCH_HOSTS"
    n <- Net.openConn
    runHandler r $ site dUrl n
