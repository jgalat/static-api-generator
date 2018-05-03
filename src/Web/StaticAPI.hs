module Web.StaticAPI (
  -- Types
  APIGeneratorOptions (..),
  Route,
  Path,
  StaticAPI,
  VarMap,
  -- Functions
  staticAPI,
  (</>),
  route,
  root,
  constant,
  variable,
  get,
  defaultOpts
) where

import  Web.StaticAPI.APIGenerator  (staticAPI)
import  Web.StaticAPI.APIGeneratorOptions (APIGeneratorOptions (..), defaultOpts)
import  Web.StaticAPI.Route         ((</>), constant, variable, route, root)
import  Web.StaticAPI.Type          (Path, Route, StaticAPI)
import  Web.StaticAPI.VarMap        (VarMap, get)
