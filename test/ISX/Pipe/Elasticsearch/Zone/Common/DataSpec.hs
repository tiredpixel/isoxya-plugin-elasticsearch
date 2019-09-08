module ISX.Pipe.Elasticsearch.Zone.Common.DataSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)
import qualified    Data.Vector                             as  V


{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec =
    describe "/data POST" $ do
        it "ok crawler-html" $ do
            res <- withSrv $ postJSON "/data" pC
            assertSuccess res
            b <- getResponseBody res
            (lines . decodeUtf8) b `shouldMatchList` [
                "POST /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/ HTTP/1.1",
                "Accept-Encoding: gzip",
                "Content-Length: 181",
                "Content-Length: 181",
                "Content-Type: application/json",
                "Host: test_echo",
                "",
                decodeUtf8 $ encode pC]
        
        it "ok link-checker" $ do
            let pC' = mergeObject pC $ object [
                    ("data", object [
                        ("meta", object [
                            ("status_code", Number 418)])])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (lines . decodeUtf8) b `shouldMatchList` [
                "POST /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/ HTTP/1.1",
                "Accept-Encoding: gzip",
                "Content-Length: 207",
                "Content-Length: 207",
                "Content-Type: application/json",
                "Host: test_echo",
                "",
                decodeUtf8 $ encode pC']
        
        it "ok spellchecker" $ do
            let pC' = object [
                    ("data", Array $ V.fromList [
                        object [
                            ("results", Array $ V.fromList [
                                object [
                                    ("status", "miss"),
                                    ("offset", Number 1),
                                    ("correct", Bool False),
                                    ("suggestions", Array $ V.fromList [
                                        "Paragraf"]),
                                    ("word", "Paragraph")],
                                object [
                                    ("status", "miss"),
                                    ("offset", Number 11),
                                    ("correct", Bool False),
                                    ("suggestions", Array $ V.fromList [
                                        "1"]),
                                    ("word", "One")]]),
                            ("paragraph", "Paragraph One.")],
                        object [
                            ("results", Array $ V.fromList [
                                object [
                                    ("status", "miss"),
                                    ("offset", Number 11),
                                    ("correct", Bool False),
                                    ("suggestions", Array $ V.fromList [
                                        "2"]),
                                    ("word", "Two")]]),
                            ("paragraph", "Paragraph Two.")]]),
                    ("site_snap", object [
                        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z")]),
                    ("url", "http://example.com:80/"),
                    ("t_retrieved", "2019-05-01T06:06:48.740524Z")]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (lines . decodeUtf8) b `shouldMatchList` [
                "POST /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/ HTTP/1.1",
                "Accept-Encoding: gzip",
                "Content-Length: 516",
                "Content-Length: 516",
                "Content-Type: application/json",
                "Host: test_echo",
                "",
                "{\"data\":[{\"status\":\"miss\",\"offset\":1,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"Paragraf\"],\"word\":\"Paragraph\"},{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"1\"],\"word\":\"One\"},{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph Two.\",\"correct\":false,\"suggestions\":[\"2\"],\"word\":\"Two\"}],\"url\":\"http://example.com:80/\",\"t_retrieved\":\"2019-05-01T06:06:48.740524Z\",\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\"}}"]


pC :: Value
pC = object [
    ("data", object []),
    ("site_snap", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z")]),
    ("url", "http://example.com:80/"),
    ("t_retrieved", "2019-05-01T06:06:48.740524Z")]
