module ISX.Plug.Elasticsearch.Zone.Apex (
    apex,
    ) where


import Data.Time.Clock
import Data.Version                 (showVersion)
import ISX.Plug.Elasticsearch.Core
import Paths_isx_plug_elasticsearch (version)


apex :: Handler b Elasticsearch ()
apex = do
    t <- liftIO getCurrentTime
    let v = toText $ showVersion version
    writeJSON $ Apex t v
