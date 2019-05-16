{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.APIGenerator where

import           Data.Aeson hiding (Options)
import           Data.Default.Class (def)
import           Data.List (intercalate)
import           Data.Map.Lazy (Map, empty, insert)
import           Data.Maybe (isJust)
import           Data.Text (Text, unpack)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (joinPath)

import           Web.StaticAPI.Internal.Types
import           Web.StaticAPI.Route
import           Web.StaticAPI.StaticResponse

staticAPI :: StaticAPI -> IO ()
staticAPI sapi = staticAPIOpts sapi def

staticAPIOpts :: StaticAPI -> Options -> IO ()
staticAPIOpts sapi options =
    let routes          = getRoutes sapi
        routesEndpoints = map genEndpoints routes
    in mapM_ (encodeEndpoints options) routesEndpoints

encodeEndpoints :: Options -> [Endpoint] -> IO ()
encodeEndpoints options = mapM_ encodeEndpoint
    where
        encodeEndpoint :: Endpoint -> IO ()
        encodeEndpoint (Endpoint path response) =
            let rootPath   = outputPath options
                folderPath = joinPath [rootPath, path]
                filePath   = joinPath [folderPath, "index.html"]
            in do
                createDirectoryIfMissing True folderPath
                encodeFile filePath response

genEndpoints :: Route -> [Endpoint]
genEndpoints (Route p sr) =
    let environments   = genEnvironments p
    in map endpoint environments
    where
        endpoint env@Environment { allPathSegments = aps } =
            Endpoint (joinPath (map unpack aps)) (runStaticResponse sr env)

genEnvironments :: Path -> [Environment]
genEnvironments (Path pss) =
    let rawPathSegments = map (\ps -> (name ps, values ps)) pss
        pairs = map (\(mn, vs) -> map ((,) mn) vs) rawPathSegments
        rawEnvironments = sequence pairs
    in map genEnv rawEnvironments
    where
        genEnv :: [(Maybe Text, Text)] -> Environment
        genEnv rawEnv = Environment
            { allPathSegments   = map snd rawEnv
            , namedPathSegments = foldl insertIf empty rawEnv
            }
        insertIf :: Map Text Text -> (Maybe Text, Text) -> Map Text Text
        insertIf m (Nothing, _) = m
        insertIf m (Just n, v)  = insert n v m
