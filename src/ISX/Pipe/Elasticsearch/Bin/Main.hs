module Main (main) where


import              ISX.Pipe.Elasticsearch.Route
import              Network.URI
import              System.Environment                      (getEnv)
import qualified    Network.HTTP.Conduit                    as  Net
import qualified    PVK.Com.API.Zone.Common.Error           as  ZE
import qualified    Snap.Http.Server                        as  Srv


main :: IO ()
main = do
    Just eUrl <- parseAbsoluteURI <$> getEnv "ELASTICSEARCH_HOSTS"
    n <- Net.newManager Net.tlsManagerSettings
    cEmp <- Srv.commandLineConfig Srv.emptyConfig
    Srv.httpServe (conf cEmp) $ site eUrl n
    where
        cLog = Srv.ConfigFileLog "-"
        conf =
            Srv.setAccessLog cLog .
            Srv.setErrorLog cLog .
            Srv.setErrorHandler ZE.intErr
