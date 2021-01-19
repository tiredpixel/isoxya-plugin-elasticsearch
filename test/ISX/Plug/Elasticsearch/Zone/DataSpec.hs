module ISX.Plug.Elasticsearch.Zone.DataSpec (spec) where


import              ISX.Test
import              Prelude                                 hiding  (get)
import qualified    Data.Vector                             as  V


spec :: Spec
spec =
    describe "create" $ do
        it "ok crawler-html" $ do
            res <- withSrv $ postJSON "/data" pC
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 620",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"crwl\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{},\"url\":\"http://example.com:80/\",\"data_i\":1,\"plug_proc\":{\"tag\":\"crawler-html\",\"href\":\"/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}",
                "{\"index\":{\"_id\":\"e0828807ee5c102320cd61034c1b1c2a180344448544244b08f5480619a4f0e4.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}"]
        
        it "ok link-checker" $ do
            let pC' = mergeObject pC $ object [
                    ("data", object [
                        ("meta", object [
                            ("status", Number 418)])]),
                    ("plug_proc", object [
                        ("tag", "link-checker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 641",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"crwl\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"meta\":{\"status\":418}},\"url\":\"http://example.com:80/\",\"data_i\":1,\"plug_proc\":{\"tag\":\"link-checker\",\"href\":\"/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}",
                "{\"index\":{\"_id\":\"e0828807ee5c102320cd61034c1b1c2a180344448544244b08f5480619a4f0e4.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}"]
        
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
                    ("plug_proc", object [
                        ("tag", "spellchecker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 2187",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"crwl\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":1,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"Paragraf\"],\"word\":\"Paragraph\"},\"url\":\"http://example.com:80/\",\"data_i\":1,\"plug_proc\":{\"tag\":\"spellchecker\",\"href\":\"/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "{\"crwl\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph One.\",\"correct\":false,\"suggestions\":[\"1\"],\"word\":\"One\"},\"url\":\"http://example.com:80/\",\"data_i\":2,\"plug_proc\":{\"tag\":\"spellchecker\",\"href\":\"/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "{\"crwl\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":{\"status\":\"miss\",\"offset\":11,\"paragraph\":\"Paragraph Two.\",\"correct\":false,\"suggestions\":[\"2\"],\"word\":\"Two\"},\"url\":\"http://example.com:80/\",\"data_i\":3,\"plug_proc\":{\"tag\":\"spellchecker\",\"href\":\"/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":3}",
                "{\"index\":{\"_id\":\"e0828807ee5c102320cd61034c1b1c2a180344448544244b08f5480619a4f0e4.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"index\":{\"_id\":\"e0828807ee5c102320cd61034c1b1c2a180344448544244b08f5480619a4f0e4.2\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}",
                "{\"index\":{\"_id\":\"e0828807ee5c102320cd61034c1b1c2a180344448544244b08f5480619a4f0e4.3\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}"]
        
        it "ok spellchecker empty-data" $ do
            let pC' = mergeObject pC $ object [
                    ("data", Array V.empty),
                    ("plug_proc", object [
                        ("tag", "spellchecker")])]
            res <- withSrv $ postJSON "/data" pC'
            assertSuccess res
            b <- getResponseBody res
            (sort . lines . decodeUtf8) b `shouldContainSubseq` [
                "Accept-Encoding: gzip",
                "Content-Length: 620",
                "Content-Type: application/x-ndjson",
                "POST /_bulk HTTP/1.1",
                "{\"crwl\":{\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z\",\"t_begin\":\"2019-05-01T06:19:54.48295Z\"},\"org\":{\"href\":\"/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd\"},\"t_retrieval\":\"2019-05-01T06:06:48.740524Z\",\"data\":[],\"url\":\"http://example.com:80/\",\"data_i\":1,\"plug_proc\":{\"tag\":\"spellchecker\",\"href\":\"/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005\"},\"site\":{\"url\":\"http://example.com:80\",\"href\":\"/site/aHR0cDovL2V4YW1wbGUuY29tOjgw\"},\"data_n\":1}",
                "{\"index\":{\"_id\":\"e0828807ee5c102320cd61034c1b1c2a180344448544244b08f5480619a4f0e4.1\",\"_index\":\"isoxya.f9b4a163-36a8-4b25-8958-d58e52a1a5bd.2019-05-01\"}}"]


pC :: Value
pC = object [
    ("data", object []),
    ("org", object [
        ("href", "/org/f9b4a163-36a8-4b25-8958-d58e52a1a5bd")]),
    ("plug_proc", object [
        ("href", "/plug_proc/8ddf53cc-6a72-11e9-8001-0242ac160005"),
        ("tag", "crawler-html")]),
    ("site", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw"),
        ("url", "http://example.com:80")]),
    ("crwl", object [
        ("href", "/site/aHR0cDovL2V4YW1wbGUuY29tOjgw/crwl/2019-05-01T06:19:54.48295Z"),
        ("t_begin", "2019-05-01T06:19:54.48295Z")]),
    ("url", "http://example.com:80/"),
    ("t_retrieval", "2019-05-01T06:06:48.740524Z")]
