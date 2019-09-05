module Main (main) where


import              ISX.Pipe.Elasticsearch.Route
import qualified    PVK.Com.API.Zone.Common.Error           as  ZE
import qualified    Snap.Http.Server                        as  Srv


main :: IO ()
main = do
    cEmp <- Srv.commandLineConfig Srv.emptyConfig
    Srv.httpServe (conf cEmp) site
    where
        cLog = Srv.ConfigFileLog "-"
        conf =
            Srv.setAccessLog cLog .
            Srv.setErrorLog cLog .
            Srv.setErrorHandler ZE.intErr
