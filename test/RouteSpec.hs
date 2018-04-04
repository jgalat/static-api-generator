module RouteSpec (spec) where

import           Test.Hspec
import           StaticAPI.Route
import           Fixture

spec :: Spec
spec = do

  describe "Route.constant" $ do
    it "should return a constant path" $ do
      let [Constant p] = constantPath
      p `shouldBe` "path"

  describe "Route.variable" $ do
    it "should return a variable path" $ do
      let [Variable name p] = variablePath2
      name `shouldBe` ":name"
      head p `shouldBe` "var1"
      last p `shouldBe` "var2"

  describe "Route.</>" $ do
    it "should concat 2 paths" $ do
      let route = constantPath </> constantSubPath
      route `shouldBe` concat [constantPath, constantSubPath]
