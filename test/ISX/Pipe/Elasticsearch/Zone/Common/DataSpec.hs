module ISX.Pipe.Elasticsearch.Zone.Common.DataSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)


spec :: Spec
spec =
    describe "/data POST" $
        it "ok" $ do
            res <- withSrv $ postJSON "/data" pC
            assertSuccess res
            -- TODO
            --b <- getResponseBody res
            --b ^? key "data" . key "status_code" . _Integer `shouldBe` Nothing
            --b ^.. key "urls" . values `shouldBe` []
            --assertElemN res 2


pC :: Value
pC = object [
    ("meta", object [
        ("url", "http://example.com:80/")]),
    ("header", object []),
    ("body", "")]
