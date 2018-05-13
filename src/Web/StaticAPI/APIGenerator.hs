{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.APIGenerator where

import           Web.StaticAPI.APIGeneratorOptions
import           Web.StaticAPI.Type
import           Web.StaticAPI.VarMap
import           Data.List
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           System.Directory

type RawPath  = String
data EndPoint = forall a . (ToJSON a) => EndPoint FilePath a

pathSeparator :: Char
pathSeparator = '/'

root :: FilePath
root = [pathSeparator]

prefixPS :: RawPath -> RawPath
prefixPS = (pathSeparator :)

concatPath :: FilePath -> FilePath -> FilePath
concatPath x y = x ++ prefixPS y

staticAPI :: StaticAPI -> APIGeneratorOptions -> IO ()
staticAPI (StaticAPIM _ routes) options =
  let trees = map endPointGenerator routes
  in  mapM_ (directoryCreator options) trees

directoryCreator :: APIGeneratorOptions -> [EndPoint] -> IO ()
directoryCreator options = mapM_ directoryCreator'
  where
    output  = outputDirectory options
    oFile   = outputFile options
    directoryCreator' :: EndPoint -> IO ()
    directoryCreator' (EndPoint fp r) = do
      let fullpath = output ++ fp
      createDirectoryIfMissing True fullpath
      BL.writeFile (concatPath fullpath oFile) (encode r)

endPointGenerator :: Route -> [EndPoint]
endPointGenerator (Route p f) = map endPointGenerator' (genVarMaps p)
  where
    endPointGenerator' :: VarMap -> EndPoint
    endPointGenerator' vm = EndPoint (genRawPath p vm) (f vm)

genRawPath :: Path -> VarMap -> RawPath
genRawPath [] _ = [pathSeparator]
genRawPath p vm = prefixPS (intercalate [pathSeparator] (map genRawPath' p))
  where
    genRawPath' :: SubPath -> RawPath
    genRawPath' (Constant v)    = v
    genRawPath' (Variable n _)  = get n vm

genVarMaps :: Path -> [VarMap]
genVarMaps p =
  let vars  = filter onlyVariables p
      pairs = map genPairs vars
  in  map fromList (sequence pairs)
  where
    onlyVariables (Variable _ _)    = True
    onlyVariables (Constant _)      = False
    genPairs :: SubPath -> [(String, RawPath)]
    genPairs (Variable name values) = map ((,) name) values
