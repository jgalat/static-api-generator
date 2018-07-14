{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.APIGenerator where

import           Data.Aeson hiding (Options)
import           Data.Default.Class (def)
import           Data.List (intercalate)
import           System.Directory (createDirectoryIfMissing)

import           Web.StaticAPI.Internal.Types
import           Web.StaticAPI.Route
import           Web.StaticAPI.StaticResponse

pathSeparator :: Char
pathSeparator = '/'

root :: FilePath
root = [pathSeparator]

prefixPS :: RawPath -> RawPath
prefixPS = (pathSeparator :)

concatPath :: FilePath -> FilePath -> FilePath
concatPath x y = x ++ prefixPS y

staticAPI :: StaticAPI -> IO ()
staticAPI sapi = staticAPIOpts sapi def

staticAPIOpts :: StaticAPI -> Options -> IO ()
staticAPIOpts sapi options =
  let routes    = getRoutes sapi
      endPoints = map endPointGenerator routes
  in  mapM_ (directoryCreator options) endPoints

directoryCreator :: Options -> [EndPoint] -> IO ()
directoryCreator options = mapM_ directoryCreator'
  where
    output  = outputDirectory options
    oFile   = outputFile options
    directoryCreator' :: EndPoint -> IO ()
    directoryCreator' (EndPoint fp r) = do
      let fullpath = output ++ fp
      createDirectoryIfMissing True fullpath
      encodeFile (concatPath fullpath oFile) r

endPointGenerator :: Route -> [EndPoint]
endPointGenerator (Route p sr) = map endPointGenerator' (genEnvironments p)
  where
    endPointGenerator' :: Environment -> EndPoint
    endPointGenerator' e =
      let rawPath         = genRawPath p e
          staticResponse  = getStaticResponse sr e
      in  EndPoint rawPath staticResponse

genRawPath :: Path -> Environment -> RawPath
genRawPath [] _ = [pathSeparator]
genRawPath p  e = prefixPS (intercalate [pathSeparator] (map genRawPath' p))
  where
    genRawPath' :: SubPath -> RawPath
    genRawPath' (Constant v)    = v
    genRawPath' (Variable n _)  = getVariable n e

genEnvironments :: Path -> [Environment]
genEnvironments p =
  let vars  = filter onlyVariables p
      pairs = map genPairs vars
  in  map fromList (sequence pairs)
  where
    onlyVariables (Variable _ _)    = True
    onlyVariables (Constant _)      = False
    genPairs :: SubPath -> [(String, RawPath)]
    genPairs (Variable name values) = map ((,) name) values
