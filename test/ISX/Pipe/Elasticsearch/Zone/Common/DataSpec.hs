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
                "PUT /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/a3e2d1be3aed1f3c5b47f3a901cd67c90abeb7b81ecfede42d7a9ffbdc1e42c5.1 HTTP/1.1",
                "Host: test_echo",
                "Content-Length: 416",
                "Accept-Encoding: gzip",
                "Content-Length: 416",
                "Content-Type: application/json",
                "",
                "{\"org_pick\":{\"tag\":\"crawler-html\",\"href\":\"/org_pick/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{},\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}"]
        
        it "ok link-checker" $ do
            let pC' = mergeObject pC $ object [
                    ("data", object [
                        ("meta", object [
                            ("status_code", Number 418)])]),
                    ("org_pick", object [
                        ("tag", "link-checker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (lines . decodeUtf8) b `shouldMatchList` [
                "PUT /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/a3e2d1be3aed1f3c5b47f3a901cd67c90abeb7b81ecfede42d7a9ffbdc1e42c5.1 HTTP/1.1",
                "Host: test_echo",
                "Content-Length: 442",
                "Accept-Encoding: gzip",
                "Content-Length: 442",
                "Content-Type: application/json",
                "",
                "{\"org_pick\":{\"tag\":\"link-checker\",\"href\":\"/org_pick/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"meta\":{\"status_code\":418}},\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}"]
        
        it "ok spellchecker" $ do
            let pC' = mergeObject pC $ object [
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
                    ("org_pick", object [
                        ("tag", "spellchecker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (lines . decodeUtf8) b `shouldMatchList` [
                "PUT /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/a3e2d1be3aed1f3c5b47f3a901cd67c90abeb7b81ecfede42d7a9ffbdc1e42c5.1 HTTP/1.1",
                "Host: test_echo",
                "Content-Length: 521",
                "Accept-Encoding: gzip",
                "Content-Length: 521",
                "Content-Type: application/json",
                "",
                "{\"org_pick\":{\"tag\":\"spellchecker\",\"href\":\"/org_pick/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":1,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"Paragraf\"],\"word\":\"Paragraph\"},\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "PUT /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/a3e2d1be3aed1f3c5b47f3a901cd67c90abeb7b81ecfede42d7a9ffbdc1e42c5.2 HTTP/1.1",
                "Host: test_echo",
                "Content-Length: 533",
                "Accept-Encoding: gzip",
                "Content-Length: 533",
                "Content-Type: application/json",
                "",
                "{\"org_pick\":{\"tag\":\"spellchecker\",\"href\":\"/org_pick/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"1\"],\"word\":\"One\"},\"url\":\"http://example.com:80/\",\"data_i\":2,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "PUT /isoxya-6c72f7c0d97098a1f5e0637265894f8061fdaa2fc0b1cb68f232cb099a781ae7-2019-05-01t06-19-54.48295z/_doc/a3e2d1be3aed1f3c5b47f3a901cd67c90abeb7b81ecfede42d7a9ffbdc1e42c5.3 HTTP/1.1",
                "Host: test_echo",
                "Content-Length: 521",
                "Accept-Encoding: gzip",
                "Content-Length: 521",
                "Content-Type: application/json",
                "",
                "{\"org_pick\":{\"tag\":\"spellchecker\",\"href\":\"/org_pick/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph Two.\",\"correct\":false,\"suggestions\":[\"2\"],\"word\":\"Two\"},\"url\":\"http://example.com:80/\",\"data_i\":3,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}"]


pC :: Value
pC = object [
    ("data", object []),
    ("org_pick", object [
        ("href", "/org_pick/8ddf53cc-6a72-11e9-8001-0242ac160005"),
        ("tag", "crawler-html")]),
    ("site", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw"),
        ("url", "http://example.com:80")]),
    ("site_snap", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z"),
        ("t_begin", "2019-05-01T06:19:54.48295Z")]),
    ("url", "http://example.com:80/"),
    ("t_retrieval", "2019-05-01T06:06:48.740524Z")]
