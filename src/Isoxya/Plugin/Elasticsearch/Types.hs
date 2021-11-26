{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


module Isoxya.Plugin.Elasticsearch.Types (
    Elasticsearch(..),
    ) where


import           Control.Lens (makeLenses)
import           Network.URI
import qualified TiredPixel.Common.Net     as N


data Elasticsearch = Elasticsearch {
    _up  :: URI,
    _net :: N.Conn}

makeLenses ''Elasticsearch
