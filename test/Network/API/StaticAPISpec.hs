{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Network.API.StaticAPISpec (main, spec) where

import           Control.Monad (forM_)
import           Data.Aeson
import           Data.Text (Text)
import           Network.Wai (Application)
import           Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import           System.Directory (removeDirectoryRecursive)
import           Test.Hspec
import           Test.Hspec.Wai

import           Network.API.StaticAPI

default (Text)

main :: IO ()
main = hspec spec

withStaticAPI :: StaticAPI () -> SpecWith Application -> Spec
withStaticAPI api =
    with (staticAPI api >> return (staticApp (defaultFileServerSettings "public"))) .
        after_ (removeDirectoryRecursive "public")

spec :: Spec
spec = do
    describe "StaticAPI" $
        describe "route" $ do
            let empty = return (toJSON ())

            withStaticAPI (route "path" empty) $
                it "adds a route for a constant path" $
                    get "/path" `shouldRespondWith` 200

            withStaticAPI (route ("path1" ./ "path2") empty) $
                it "adds a route when composing two constant path segments" $
                    get "/path1/path2" `shouldRespondWith` 200

            withStaticAPI (route ["path1", "path2"] empty) $
                it "adds a route for each possible path segment when using variable path segments" $
                    forM_ ["/path1", "/path2"] $ \path ->
                        get path `shouldRespondWith` 200

            withStaticAPI (route ("path" ./ ["path1", "path2"]) empty) $
                it "adds a route for each possible path when composing constant and variable path segments" $
                    forM_ ["/path/path1", "/path/path2"] $ \path ->
                        get path `shouldRespondWith` 200

            withStaticAPI (route (["path1", "path2"] ./ ["path1", "path2"]) empty) $
                it "adds a route for each possible path when composing variable path segments" $
                    forM_ [ "/path1/path1"
                          , "/path1/path2"
                          , "/path2/path1"
                          , "/path2/path2"
                          ] $ \path ->
                        get path `shouldRespondWith` 200

    describe "StaticResponse" $ do
        withStaticAPI (route "path" $ return (object ["bool" .= True])) $
            it "encodes the static response with its ToStaticResponse representation" $
                get "/path" `shouldRespondWith` "{\"bool\":true}" { matchStatus = 200 }

        describe "pathSegments" $ do
            withStaticAPI (route ("path1" ./ "path2") (toJSON <$> pathSegments)) $
                it "provides with the constant path segments of the route" $
                    get "/path1/path2" `shouldRespondWith` "[\"path1\",\"path2\"]" { matchStatus = 200 }

            withStaticAPI (route ["path1", "path2"] (toJSON <$> pathSegments)) $
                it "provides with the variable path segments for each route" $ do
                    get "/path1" `shouldRespondWith` "[\"path1\"]" { matchStatus = 200 }
                    get "/path2" `shouldRespondWith` "[\"path2\"]" { matchStatus = 200 }

            withStaticAPI (route ("path" ./ ["path1", "path2"]) (toJSON <$> pathSegments)) $
                it "provides with the constant and variable path segments for each route" $ do
                    get "/path/path1" `shouldRespondWith` "[\"path\",\"path1\"]" { matchStatus = 200 }
                    get "/path/path2" `shouldRespondWith` "[\"path\",\"path2\"]" { matchStatus = 200 }

        describe "getPathSegment" $
            withStaticAPI (route ("name" .> ["path1", "path2"]) (toJSON <$> getPathSegment "name")) $
                it "provides with the named path segment for each route" $ do
                    get "/path1" `shouldRespondWith` "\"path1\"" { matchStatus = 200 }
                    get "/path2" `shouldRespondWith` "\"path2\"" { matchStatus = 200 }