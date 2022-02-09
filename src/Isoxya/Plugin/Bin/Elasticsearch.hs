{-# LANGUAGE TemplateHaskell #-}


module Main (main) where


import           Control.Concurrent                (forkIO)
import           Control.Lens                      (makeLenses)
import           Data.Version                      (showVersion)
import           Isoxya.Plugin.Elasticsearch
import           Network.URI
import           Paths_isoxya_plugin_elasticsearch (version)
import           Snap.Snaplet
import           System.IO
import qualified TiredPixel.Common.Net             as N
import qualified TiredPixel.Common.Snap.Main       as S


newtype App = App { _elasticsearch :: Snaplet Elasticsearch }

makeLenses ''App

main :: IO ()
main = do
    hPutStrLn stderr $ "Isoxya plugin Elasticsearch " <> toString ver
    Just u <- parseAbsoluteURI . fromMaybe uDef <$>
        lookupEnv "ELASTICSEARCH_HOST"
    done <- S.init
    tId <- forkIO $ do
        n <- N.openConn
        serveSnaplet S.config $ initApp u n
    S.wait done tId
    where
        uDef = "http://elastic:password@es:9200"
        ver = toText $ showVersion version


initApp :: URI -> N.Conn -> SnapletInit App App
initApp u n = makeSnaplet "App" "" Nothing $ do
    elasticsearch' <- nestSnaplet "" elasticsearch $ initElasticsearch u n
    return $ App elasticsearch'
