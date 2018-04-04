module Main (main) where

import           Test.Hspec
import qualified RouteSpec
import qualified APIGeneratorSpec

main :: IO ()
main = hspec $ do
  RouteSpec.spec
  APIGeneratorSpec.spec
