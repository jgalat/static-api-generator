module APIGeneratorSpec (spec) where

import           Test.Hspec
import           Web.StaticAPI.APIGenerator

spec :: Spec
spec =
  describe "APIGenerator" $
    describe "APIGenerator.staticAPI" $
      it "should generate the raw directories to simulate an api" $
        pendingWith "Write me"
