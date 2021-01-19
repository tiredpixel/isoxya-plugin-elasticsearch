module ISX.Test (
    module TPX.Com.API.Test,
    withSrv,
    ) where


import              ISX.Plug.Elasticsearch.Route
import              Network.URI
import              System.Environment                      (lookupEnv)
import              TPX.Com.API.Test
import qualified    TPX.Com.Net                             as  N


withSrv :: RequestBuilder IO () -> IO Response
withSrv r = do
    Just u <- parseAbsoluteURI . fromMaybe uDef <$>
        lookupEnv "_TEST_ELASTICSEARCH_HOST"
    n <- N.openConn
    runHandler r $ site u n
    where
        uDef = "http://test_echo"
