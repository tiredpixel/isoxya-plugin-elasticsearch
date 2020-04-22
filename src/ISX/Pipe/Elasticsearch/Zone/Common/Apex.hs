module ISX.Pipe.Elasticsearch.Zone.Common.Apex (
    apex
    ) where


import              Snap.Core
import              Snap.Extras.JSON
import qualified    Data.Text                               as  T
import qualified    Data.Time.Clock                         as  Clock
import qualified    ISX.Pipe.Elasticsearch.Resource.Common  as  R


apex :: Snap ()
apex = do
    t <- liftIO Clock.getCurrentTime
    version <- liftIO (T.stripEnd <$> readFileText ".version")
    writeJSON $ R.Apex t version
