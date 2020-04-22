module ISX.Pipe.Elasticsearch.Zone.Common.ApexSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)
import qualified    Data.Map                                as  M
import qualified    Data.Text                               as  T


spec :: Spec
spec =
    describe "/ GET" $
        it "ok" $ do
            version <- liftIO (T.stripEnd <$> readFileText ".version")
            res <- withSrv $ get "/" pR
            assertSuccess res
            b <- getResponseBody res
            toString (b ^. key "t_now" . _String) `shouldContain` "T"
            b ^. key "version" . _String `shouldBe` version
            assertElemN res 2


pR :: Params
pR = M.empty
