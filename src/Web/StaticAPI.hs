module Web.StaticAPI
  ( -- Types
    StaticAPI
  , StaticResponse
  , Path
  , Options (..)
    -- Path constructors
  , (./)
  , (.>)
    -- Static response
  , getPathSegment
  , pathSegments
    -- Static API
  , staticAPI
  , staticAPIOpts
  , route
  ) where

import  Web.StaticAPI.APIGenerator    ( staticAPI
                                      , staticAPIOpts )

import  Web.StaticAPI.Route           ( route )

import  Web.StaticAPI.StaticResponse  ( getPathSegment, pathSegments )

import  Web.StaticAPI.Internal.Types  ( Options (..)
                                      , Path
                                      , StaticAPI
                                      , StaticResponse
                                      , (./)
                                      , (.>)
                                      )