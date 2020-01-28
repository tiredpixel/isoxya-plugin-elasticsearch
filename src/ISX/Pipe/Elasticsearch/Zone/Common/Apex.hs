module ISX.Pipe.Elasticsearch.Zone.Common.Apex (
    apex
    ) where


import              Snap.Core
import              Snap.Extras.JSON
import              System.Environment                      (lookupEnv)
import qualified    Data.Time.Clock                         as  Clock
import qualified    ISX.Pipe.Elasticsearch.Resource.Common  as  R


apex :: Snap ()
apex = do
    t <- liftIO Clock.getCurrentTime
    version_ <- liftIO $ join <$> (fmap . fmap) readMaybe (lookupEnv "VERSION")
    let version = fromMaybe versionDef version_
    writeJSON $ R.Apex t version


versionDef :: Text
versionDef = "0.0.0"
