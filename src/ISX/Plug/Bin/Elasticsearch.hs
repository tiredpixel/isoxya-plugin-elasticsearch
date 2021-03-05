{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import           Control.Concurrent           (forkIO)
import           Control.Lens                 (makeLenses)
import           Data.Version                 (showVersion)
import           ISX.Plug.Elasticsearch
import           Network.URI
import           Paths_isx_plug_elasticsearch (version)
import           Snap.Snaplet
import           System.Environment           (lookupEnv)
import           System.IO
import           TPX.Com.Snap.CoreUtils
import qualified TPX.Com.Net                  as N


newtype App = App {
    _elasticsearch :: Snaplet Elasticsearch}

makeLenses ''App

main :: IO ()
main = do
    let ver = toText $ showVersion version
    hPutStrLn stderr $ "Isoxya plugin: Elasticsearch " <> toString ver
    Just u <- parseAbsoluteURI . fromMaybe uDef <$>
        lookupEnv "ELASTICSEARCH_HOST"
    tId <- forkIO $ do
        n <- N.openConn
        serveSnaplet snapCfg $ initApp u n
    snapWait tId
    where
        uDef = "http://elastic:password@es:9200"


initApp :: URI -> N.Conn -> SnapletInit App App
initApp u n = makeSnaplet "App" "" Nothing $ do
    elasticsearch' <- nestSnaplet "" elasticsearch $ initElasticsearch u n
    return $ App elasticsearch'
