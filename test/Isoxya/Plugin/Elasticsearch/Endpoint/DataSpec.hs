module Isoxya.Plugin.Elasticsearch.Endpoint.DataSpec (spec) where


import           Isoxya.Plugin.Elasticsearch.Test
import           TiredPixel.Common.Isoxya.Streamer
import qualified Data.ByteString.Lazy.Char8        as C8


spec :: Spec
spec = snapElasticsearch $
    describe "create" $ do
        it "crawler-html => 200" $ do
            (i, dataE) <- load "example.com" "crawler-html" "" Null
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE
        
        it "link-checker => 200" $ do
            (i, dataE) <- load "example.com" "link-checker" "" $ object [
                ("meta", object [
                    ("status", Number 418)])]
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE
        
        it "spellchecker => 200" $ do
            (i, dataE) <- load "example.com" "spellchecker" "" $ array [
                object [
                    ("results", array [
                        object [
                            ("status", "miss"),
                            ("offset", Number 1),
                            ("correct", Bool False),
                            ("suggestions", array [
                                "Paragraf"]),
                            ("word", "Paragraph")],
                        object [
                            ("status", "miss"),
                            ("offset", Number 11),
                            ("correct", Bool False),
                            ("suggestions", array [
                                "1"]),
                            ("word", "One")]]),
                    ("paragraph", "Paragraph One.")],
                object [
                    ("results", array [
                        object [
                            ("status", "miss"),
                            ("offset", Number 11),
                            ("correct", Bool False),
                            ("suggestions", array [
                                "2"]),
                            ("word", "Two")]]),
                    ("paragraph", "Paragraph Two.")]]
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE
        
        it "spellchecker empty data => 200" $ do
            (i, dataE) <- load "example.com" "spellchecker" "-empty-data" Null
            let req = postJSON "/data" i
            res <- runRequest req
            test res dataE


load :: MonadIO m => Text -> Text -> Text -> Value -> m (Streamer, [Value])
load url tag sfx dat = do
    let i = genStreamer url tag dat
    b <- readFileLBS $ fixtureResult url (tag <> sfx)
    let Just dataE = sequence (decode <$> C8.lines b)
    return (i, dataE)

test :: Response -> [Value] -> SnapHspecM b ()
test res dataE = do
    rspStatus res `shouldBe` 200
    b <- getResponseBody res
    let b' = snd $ splitHeaderBody' b
    let Just dataA = sequence (decode <$> C8.lines (fromStrict b'))
    dataA `shouldBeListJSON` dataE
