module StaticAPI.Type where

import           StaticAPI.Route

data StaticAPI = StaticAPI [Route]

staticAPI :: [Route] -> StaticAPI
staticAPI = StaticAPI
