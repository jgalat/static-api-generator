module Network.API.StaticAPI.Internal.StaticResponse
    ( URIParts
    , toURIPartsSequence
    ) where

import           Data.Text (Text)

import           Network.API.StaticAPI.Path

import           Network.API.StaticAPI.Internal.PathSegment

type URIParts = [(Maybe Text, Text)]

toURIPartsSequence :: Path -> [URIParts]
toURIPartsSequence (Path ps) =
    let parts = map toURIParts ps
    in sequence parts
    where
        toURIParts :: PathSegment -> URIParts
        toURIParts (maybeNamed, values) = map ((,) maybeNamed) values