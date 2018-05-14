module Web.StaticAPI (
  -- Types
    Options (..)
  , Path
  , StaticAPI
  , StaticResponse
  -- Functions
  , staticAPI
  , staticAPIOpts
  , (./)
  , root
  , constant
  , variable
  , route
  , readVariable
  ) where

import  Web.StaticAPI.APIGenerator    ( staticAPI
                                      , staticAPIOpts )
import  Web.StaticAPI.Route           ( (./)
                                      , root
                                      , constant
                                      , variable
                                      , route )

import  Web.StaticAPI.StaticResponse  ( readVariable )

import  Web.StaticAPI.Internal.Types  ( Options (..)
                                      , Path
                                      , StaticAPI
                                      , StaticResponse )
