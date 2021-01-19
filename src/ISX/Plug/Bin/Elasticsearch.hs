module Main (main) where


import              ISX.Plug.Elasticsearch.Route
import              Network.URI
import              System.Environment                      (getEnv)
import qualified    Snap.Http.Server                        as  Srv
import qualified    TPX.Com.API.Res                         as  Res
import qualified    TPX.Com.Net                             as  Net


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
