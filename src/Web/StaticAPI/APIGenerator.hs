{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.APIGenerator where

import           Web.StaticAPI.APIGeneratorOptions
import           Web.StaticAPI.Type
import           Web.StaticAPI.VarMap
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           System.Directory

type Schema   = String
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

concatRawPath :: RawPath -> RawPath -> RawPath
concatRawPath = concatPath

concatSchema :: Schema -> Schema -> Schema
concatSchema = concatPath

staticAPI :: StaticAPI -> APIGeneratorOptions -> IO ()
staticAPI (StaticAPIM _ routes) options =
  let trees = map endPointGenerator routes
  in  mapM_ (directoryCreator options) trees

directoryCreator :: APIGeneratorOptions -> [EndPoint] -> IO ()
directoryCreator options = mapM_ directoryCreator'
  where
    output  = outputDirectory options
    oFile   = outputFile options
    directoryCreator' (EndPoint fp r) = do
      let fullpath = output ++ fp
      createDirectoryIfMissing True fullpath
      BL.writeFile (concatPath fullpath oFile) (encode r)

endPointGenerator :: Route -> [EndPoint]
endPointGenerator (Route [] f) = [EndPoint root (f empty)]
endPointGenerator (Route ps f) =
  let rawPaths = genRawPaths ps
      schema = genSchema ps
  in do
    rawPath <- rawPaths
    return (EndPoint rawPath (f (genVarMap rawPath schema)))

genRawPaths :: Path -> [RawPath]
genRawPaths [] = [root]
genRawPaths ps = map prefixPS (genRawPaths' ps)
  where
    genRawPaths' :: Path -> [RawPath]
    genRawPaths' [] = undefined
    genRawPaths' [Constant constant]     = [constant]
    genRawPaths' [Variable _ variables]  = variables
    genRawPaths' (Constant path : xs)    = do
      routes <- genRawPaths' xs
      return (concatRawPath path routes)
    genRawPaths' (Variable _ paths : xs) = do
      path    <- paths
      routes  <- genRawPaths' xs
      return (concatRawPath path routes)

genSchema :: Path -> Schema
genSchema [] = root
genSchema ps = root ++ genSchema' ps
  where
    genSchema' :: Path -> Schema
    genSchema' [] = undefined
    genSchema' [Constant path]         = path
    genSchema' [Variable name _]       = name
    genSchema' (Constant path : xs)    = concatSchema path (genSchema' xs)
    genSchema' (Variable name _ : xs)  = concatSchema name (genSchema' xs)

genVarMap :: RawPath -> Schema -> VarMap
genVarMap rp sc = genVarMap' (tail rp) (tail sc) []
  where
    notPathSeparator = (/= pathSeparator)
    getSubPath      = takeWhile notPathSeparator
    getVariableKey  = drop 1
    isVariable      = (== ':') . head
    dropSubPath     = drop 1 . dropWhile notPathSeparator
    genVarMap' :: RawPath -> Schema -> [(String, String)] -> VarMap
    genVarMap' "" "" acc = fromList acc
    genVarMap' rp sc acc =
      let rsp     = getSubPath rp
          ssp     = getSubPath sc
          restRP  = dropSubPath rp
          restSC  = dropSubPath sc
      in if isVariable ssp
          then  let k  = getVariableKey ssp
                    kv = (k, rsp)
                in genVarMap' restRP restSC (kv:acc)
          else genVarMap' restRP restSC acc
