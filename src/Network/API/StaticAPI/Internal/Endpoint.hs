{-# LANGUAGE GADTs #-}
module Network.API.StaticAPI.Internal.Endpoint
    ( Endpoint (..)
    , toEndpointSequence
    ) where

import           Data.Text (unpack)
import           System.FilePath (joinPath)

import           Network.API.StaticAPI.StaticResponse

import           Network.API.StaticAPI.Internal.Route
import           Network.API.StaticAPI.Internal.StaticResponse

data Endpoint where
    Endpoint :: (ToStaticResponse a)
                => FilePath
                -> a
                -> Endpoint

toEndpointSequence :: Monad m => Route m -> m [Endpoint]
toEndpointSequence (Route p sr) = mapM endpoint (toURIPartsSequence p)
    where
        endpoint uriParts = do
            staticResponse <- runStaticResponseT sr uriParts
            return (Endpoint (joinPath (map (unpack . snd) uriParts)) staticResponse)
