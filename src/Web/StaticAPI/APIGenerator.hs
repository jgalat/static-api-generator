{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.APIGenerator where

import           Web.StaticAPI.APIGeneratorOptions
import           Web.StaticAPI.Route
import           Web.StaticAPI.Type
import           Web.StaticAPI.VarMap
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           System.Directory

type Schema   = String
type RawPath  = String
data EndPoint = forall a . (ToJSON a) => EndPoint FilePath a

concat' :: String -> String -> String
concat' x y = x ++ ('/' : y)

index :: FilePath
index = "/index"

staticAPI :: StaticAPI -> APIGeneratorOptions -> IO ()
staticAPI (StaticAPIM _ routes) options =
  let trees = map endPointGenerator routes
  in  mapM_ (directoryCreator options) trees

directoryCreator :: APIGeneratorOptions -> [EndPoint] -> IO ()
directoryCreator options = mapM_ directoryCreator'
  where
    outputDirectory = output options
    ext             = extension (fileFormat options)
    indexFile       = index ++ ext
    directoryCreator' (EndPoint fp r) = do
      let fullpath = outputDirectory ++ fp
      createDirectoryIfMissing True fullpath
      BL.writeFile (fullpath ++ indexFile) (encode r)

endPointGenerator :: Route -> [EndPoint]
endPointGenerator (Route [] _) = undefined
endPointGenerator (Route ps f) =
  let rawPaths = genRawPaths ps
      schema = genSchema ps
  in do
    rawPath <- rawPaths
    return (EndPoint rawPath (f (genVarMap rawPath schema)))

genRawPaths :: Path -> [RawPath]
genRawPaths ps = map ('/' :) (genRawPaths' ps)
  where
    genRawPaths' :: Path -> [RawPath]
    genRawPaths' [] = undefined
    genRawPaths' [Constant constant]     = [constant]
    genRawPaths' [Variable _ variables]  = variables
    genRawPaths' (Constant path : xs)    = do
      routes <- genRawPaths' xs
      return (concat' path routes)
    genRawPaths' (Variable _ paths : xs) = do
      path    <- paths
      routes  <- genRawPaths' xs
      return (concat' path routes)

genSchema :: Path -> Schema
genSchema ps = '/' : genSchema' ps
  where
    genSchema' :: Path -> Schema
    genSchema' [] = undefined
    genSchema' [Constant path]         = path
    genSchema' [Variable name _]       = name
    genSchema' (Constant path : xs)    = concat' path (genSchema' xs)
    genSchema' (Variable name _ : xs)  = concat' name (genSchema' xs)

genVarMap :: RawPath -> Schema -> VarMap
genVarMap rp sc = genVarMap' (tail rp) (tail sc) []
  where
    getSubPath      = takeWhile (/='/')
    getVariableKey  = drop 1
    isVariable      = (==':') . head
    dropSubPath     = drop 1 . dropWhile (/='/')
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
