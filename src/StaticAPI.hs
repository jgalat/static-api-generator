module StaticAPI (
  -- Types
  Route,
  StaticAPI,
  VarMap,
  -- Functions
  staticAPI,
  runStaticAPI,
  (</>),
  route,
  constant,
  variable,
  get
) where

import  StaticAPI.APIGenerator  (runStaticAPI)
import  StaticAPI.Route         (Route, (</>), route, constant, variable)
import  StaticAPI.Type          (StaticAPI, staticAPI)
import  StaticAPI.VarMap        (VarMap, get)
