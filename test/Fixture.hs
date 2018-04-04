module Fixture where

import           StaticAPI.Route

--
-- Paths
--
constantPath        = constant "path"
constantSubPath     = constant "subpath"
constantCustomPath  = constant "custom"
variablePath2       = variable "name" ["var1", "var2"]
variablePath3       = variable "name" ["var1", "var2", "var3"]
