module Web.StaticAPI.Route where

import           Data.Aeson
import           Web.StaticAPI.Type
import           Web.StaticAPI.VarMap

(./) :: Path -> Path -> Path
(./) = (++)

root :: Path
root = []

constant :: String -> Path
constant path = [Constant path]

variable :: String -> [String] -> Path
variable name paths = [Variable name paths]

addRoute :: Route -> StaticAPIM ()
addRoute r = StaticAPIM () [r]

route :: ToJSON a => Path -> (VarMap -> a) -> StaticAPI
route p f = addRoute (Route p f)
