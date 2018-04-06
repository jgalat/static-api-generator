module Web.StaticAPI (
  -- Types
  APIGeneratorOptions (..),
  DirectoryFormat (..),
  FileFormat (..),
  Route,
  Path,
  StaticAPI,
  VarMap,
  -- Functions
  staticAPI,
  (</>),
  route,
  constant,
  variable,
  root,
  get,
  defaultOpts
) where

import  Web.StaticAPI.APIGenerator  (staticAPI)
import  Web.StaticAPI.APIGeneratorOptions (FileFormat (..), DirectoryFormat (..), APIGeneratorOptions (..), defaultOpts)
import  Web.StaticAPI.Route         ((</>), constant, variable, route, root)
import  Web.StaticAPI.Type          (Path, Route, StaticAPI)
import  Web.StaticAPI.VarMap        (VarMap, get)
