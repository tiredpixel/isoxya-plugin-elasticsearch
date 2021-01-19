module Main (main) where


import              Data.Version                            (showVersion)
import              ISX.Plug.Elasticsearch.Route
import              Network.URI
import              Paths_isx_plug_elasticsearch            (version)
import              System.Environment                      (lookupEnv)
import              TPX.Com.API.Res
import qualified    Snap.Http.Server                        as  Srv
import qualified    TPX.Com.Net                             as  N


main :: IO ()
main = do
    let ver = toText $ showVersion version
    putTextLn ver
    Just u <- parseAbsoluteURI . fromMaybe uDef <$>
        lookupEnv "ELASTICSEARCH_HOST"
    n <- N.openConn
    cEmp <- Srv.commandLineConfig Srv.emptyConfig
    Srv.httpServe (conf cEmp) $ site u n
    where
        cLog = Srv.ConfigFileLog "-"
        conf =
            Srv.setAccessLog cLog .
            Srv.setErrorLog cLog .
            Srv.setErrorHandler intErr'
        uDef = "http://elastic:password@es:9200"
