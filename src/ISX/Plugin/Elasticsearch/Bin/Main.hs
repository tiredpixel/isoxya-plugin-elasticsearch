module Main (main) where


import              ISX.Plugin.Elasticsearch.Route
import              Network.URI
import              System.Environment                      (getEnv)
import qualified    PVK.Com.API.Res                         as  Res
import qualified    PVK.Com.Net                             as  Net
import qualified    Snap.Http.Server                        as  Srv


main :: IO ()
main = do
    Just dUrl <- parseAbsoluteURI <$> getEnv "ELASTICSEARCH_HOST"
    n <- Net.openConn
    cEmp <- Srv.commandLineConfig Srv.emptyConfig
    Srv.httpServe (conf cEmp) $ site dUrl n
    where
        cLog = Srv.ConfigFileLog "-"
        conf =
            Srv.setAccessLog cLog .
            Srv.setErrorLog cLog .
            Srv.setErrorHandler Res.intErr'
