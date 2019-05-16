module Web.StaticAPI.Route where

import           Data.Aeson (ToJSON)
import           Control.Monad.Writer (execWriter, tell)

import           Web.StaticAPI.Internal.Types

addRoute :: Route -> StaticAPIM ()
addRoute r = StaticAPIM (tell [r])

route :: (PathComponent a, ToJSON b) => a -> StaticResponse b -> StaticAPI
route p sr = addRoute (Route (toPath p) sr)

getRoutes :: StaticAPIM a -> [Route]
getRoutes = execWriter . runSAPI

