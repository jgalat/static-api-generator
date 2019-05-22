{-# LANGUAGE GADTs #-}
module Network.API.StaticAPI.Internal.Route
    ( Route (..)
    ) where

import            Network.API.StaticAPI.Path
import            Network.API.StaticAPI.StaticResponse

data Route m where
  Route :: (Monad m, ToStaticResponse a)
        => Path
        -> StaticResponseT m a
        -> Route m
