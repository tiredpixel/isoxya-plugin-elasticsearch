module Isoxya.Plugin.Elasticsearch.Endpoint.Apex (
    apex,
    ) where


import           Data.Time.Clock
import           Data.Version                      (showVersion)
import           Isoxya.Plugin.Elasticsearch.Core
import           Paths_isoxya_plugin_elasticsearch (version)


apex :: Handler b Elasticsearch ()
apex = do
    t <- liftIO getCurrentTime
    writeJSON $ Apex t ver
    where
        ver = toText $ showVersion version
