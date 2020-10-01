module ISX.Plug.Elasticsearch.Zone.Common.DataSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)
import qualified    Data.Vector                             as  V


spec :: Spec
spec =
    describe "/data POST" $ do
        it "ok crawler-html" $ do
            res <- withSrv $ postJSON "/data" pC
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 628",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"index\":{\"_id\":\"9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{},\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org_proc\":{\"tag\":\"crawler-html\",\"href\":\"/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}"]
        
        it "ok link-checker" $ do
            let pC' = mergeObject pC $ object [
                    ("data", object [
                        ("meta", object [
                            ("status_code", Number 418)])]),
                    ("org_proc", object [
                        ("tag", "link-checker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 654",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"index\":{\"_id\":\"9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"meta\":{\"status_code\":418}},\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org_proc\":{\"tag\":\"link-checker\",\"href\":\"/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}"]
        
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
                    ("org_proc", object [
                        ("tag", "spellchecker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 2211",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"index\":{\"_id\":\"9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"index\":{\"_id\":\"9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.2\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"index\":{\"_id\":\"9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.3\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":1,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"Paragraf\"],\"word\":\"Paragraph\"},\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org_proc\":{\"tag\":\"spellchecker\",\"href\":\"/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "{\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"1\"],\"word\":\"One\"},\"url\":\"http://example.com:80/\",\"data_i\":2,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org_proc\":{\"tag\":\"spellchecker\",\"href\":\"/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "{\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph Two.\",\"correct\":false,\"suggestions\":[\"2\"],\"word\":\"Two\"},\"url\":\"http://example.com:80/\",\"data_i\":3,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org_proc\":{\"tag\":\"spellchecker\",\"href\":\"/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}"]
        
        it "ok spellchecker empty-data" $ do
            let pC' = mergeObject pC $ object [
                    ("data", Array V.empty),
                    ("org_proc", object [
                        ("tag", "spellchecker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 628",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"index\":{\"_id\":\"9c8100c7642a06acc892c9696e55789ec0dd67ad0dee06a5c378343b5e47a969.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":[],\"url\":\"http://example.com:80/\",\"data_i\":1,\"site_snap\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org_proc\":{\"tag\":\"spellchecker\",\"href\":\"/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}"]


pC :: Value
pC = object [
    ("data", object []),
    ("org", object [
        ("href", "/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd")]),
    ("org_proc", object [
        ("href", "/org_proc/8ddf53cc-6a72-11e9-8001-0242ac160005"),
        ("tag", "crawler-html")]),
    ("site", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw"),
        ("url", "http://example.com:80")]),
    ("site_snap", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/site_snap/2019-05-01T06:19:54.48295Z"),
        ("t_begin", "2019-05-01T06:19:54.48295Z")]),
    ("url", "http://example.com:80/"),
    ("t_retrieval", "2019-05-01T06:06:48.740524Z")]
