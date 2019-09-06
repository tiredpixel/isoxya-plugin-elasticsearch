module ISX.Test (
    module PVK.Com.API.Test,
    withSrv
    ) where


import              ISX.Pipe.Elasticsearch.Route
import              Network.URI
import              PVK.Com.API.Test
import              System.Environment                      (getEnv)
import qualified    Network.HTTP.Conduit                    as  Net


withSrv :: RequestBuilder IO () -> IO Response
withSrv r = do
    Just eUrl <- parseAbsoluteURI <$> getEnv "_TEST_ELASTICSEARCH_HOSTS"
    n <- Net.newManager Net.tlsManagerSettings
    runHandler r $ site eUrl n
