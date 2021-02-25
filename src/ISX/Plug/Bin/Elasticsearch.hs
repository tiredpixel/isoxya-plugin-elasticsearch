{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


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
    hPutStrLn stderr $ toString ver
    Just u <- parseAbsoluteURI . fromMaybe uDef <$>
        lookupEnv "ELASTICSEARCH_HOST"
    n <- N.openConn
    serveSnaplet snapCfg $ initApp u n
    where
        uDef = "http://elastic:password@es:9200"


initApp :: URI -> N.Conn -> SnapletInit App App
initApp u n = makeSnaplet "app" "Isoxya plugin: Elasticsearch" Nothing $ do
    elasticsearch' <- nestSnaplet "" elasticsearch $ initElasticsearch u n
    return $ App elasticsearch'
