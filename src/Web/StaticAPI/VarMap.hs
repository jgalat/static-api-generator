module Web.StaticAPI.VarMap where

import qualified Data.Map.Lazy as Map
import           Data.Maybe

type VarMap = Map.Map String String

empty :: VarMap
empty = Map.empty

fromList :: [(String, String)] -> VarMap
fromList = Map.fromList

get :: String -> VarMap -> String
get k vm =
  maybe
    (error "StaticAPI: given key (" ++ k ++ ") couldn't be found in the map.")
    (id)
    (Map.lookup k vm)
