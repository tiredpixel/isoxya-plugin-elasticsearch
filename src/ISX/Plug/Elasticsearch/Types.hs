{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module ISX.Plug.Elasticsearch.Types (
    Elasticsearch(..),
    ) where


import           Control.Lens (makeLenses)
import           Network.URI
import qualified TPX.Com.Net               as N


data Elasticsearch = Elasticsearch {
    _up  :: URI,
    _net :: N.Conn}

makeLenses ''Elasticsearch
