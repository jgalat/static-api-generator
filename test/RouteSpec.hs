module RouteSpec (spec) where

import           Test.Hspec
import           Web.StaticAPI.Route
import           Web.StaticAPI.Type

constantPath        = constant "path"
constantPath2       = constant "path2"
constantSubPath     = constant "subpath"
constantCustomPath  = constant "custom"
variablePath2       = variable "name" ["var1", "var2"]

spec :: Spec
spec =
  describe "Route" $ do
    describe "Route.root" $
      it "should return a constant empty path" $ do
        root `shouldBe` []

    describe "Route.constant" $
      it "should return a constant path" $ do
        let [Constant p] = constantPath
        p `shouldBe` "path"

    describe "Route.variable" $
      it "should return a variable path" $ do
        let [Variable name p] = variablePath2
        name `shouldBe` ":name"
        elem "var1" p `shouldBe` True
        elem "var2" p `shouldBe` True
        length p `shouldBe` 2

    describe "Route.(./)" $
      it "should concat 2 paths" $ do
        let route = constantPath ./ constantSubPath
        route `shouldBe` (constantPath ++ constantSubPath)

    describe "Route.route" $
      it "should return a static api with one route" $ do
        let StaticAPIM () rs = route constantPath (const ())
        length rs `shouldBe` 1
