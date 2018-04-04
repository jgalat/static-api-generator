module APIGeneratorSpec (spec) where

import           Test.Hspec
import           StaticAPI.Route
import           StaticAPI.APIGenerator
import           Fixture

spec :: Spec
spec = do

  describe "APIGenerator.genRawPaths" $ do
    it "should concat 2 constant paths" $ do
      let generated = genRawPaths (constantPath </> constantSubPath)
      generated `shouldBe` ["/path/subpath"]

    it "should generate a route for each variable in a variable path" $ do
      let generated = genRawPaths (constantPath </> variablePath2)
      generated `shouldBe` ["/path/var1", "/path/var2"]

    it "should generate a route for each variable path followed by a constant path" $ do
      let generated = genRawPaths (constantPath </> variablePath2 </> constantCustomPath)
      generated `shouldBe` ["/path/var1/custom", "/path/var2/custom"]

  describe "APIGenerator.genSchema" $ do
    it "should concat 2 constant paths" $ do
      let generated = genSchema (constantPath </> constantSubPath)
      generated `shouldBe` "/path/subpath"

    it "should replace variable paths with its key name" $ do
      let generated = genSchema (constantPath </> variablePath2)
      generated `shouldBe` "/path/:name"
