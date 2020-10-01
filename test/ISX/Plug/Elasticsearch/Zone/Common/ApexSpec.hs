module ISX.Plug.Elasticsearch.Zone.Common.ApexSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)
import qualified    Data.Map                                as  M


spec :: Spec
spec =
    describe "/ GET" $
        it "ok" $ do
            res <- withSrv $ get "/" pR
            assertSuccess res
            b <- getResponseBody res
            toString (b ^. key "t_now" . _String) `shouldContain` "T"
            b ^. key "version" . _String `shouldBe` "0.0.0"
            assertElemN res 2


pR :: Params
pR = M.empty
