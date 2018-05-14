module Web.StaticAPI.Route where

import           Data.Aeson (ToJSON)
import           Control.Monad.Writer (execWriter, tell)

import           Web.StaticAPI.Internal.Types

(./) :: Path -> Path -> Path
(./) = (++)

root :: Path
root = []

constant :: String -> Path
constant path = [Constant path]

variable :: String -> [String] -> Path
variable name paths = [Variable name paths]

addRoute :: Route -> StaticAPIM ()
addRoute r = StaticAPIM (tell [r])

getRoutes :: StaticAPIM a -> [Route]
getRoutes = execWriter . runSAPI

route :: ToJSON a => Path -> StaticResponse a -> StaticAPI
route p sr = addRoute (Route p sr)
