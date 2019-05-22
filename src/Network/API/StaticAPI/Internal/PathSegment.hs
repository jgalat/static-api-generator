module Network.API.StaticAPI.Internal.PathSegment
    ( PathSegment
    , pathSegment
    ) where

import           Data.Text (Text)

type PathSegment = (Maybe Text, [Text])

pathSegment :: Maybe Text -> [Text] -> PathSegment
pathSegment = (,)
