{-# LANGUAGE ExistentialQuantification #-}
module StaticAPI.Route where

import           Data.Aeson
import           StaticAPI.VarMap

data SubPath  = Constant String
              | Variable String [String]
              deriving (Eq, Show)

type Path     = [SubPath]

data Route = forall a . (ToJSON a) => Route Path (VarMap -> a)

(</>) :: Path -> Path -> Path
(</>) = (++)

constant :: String -> Path
constant path = [Constant path]

variable :: String -> [String] -> Path
variable name paths = [Variable (':':name) paths]

route :: ToJSON a => Path -> (VarMap -> a) -> Route
route = Route
