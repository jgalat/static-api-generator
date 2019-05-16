module Web.StaticAPI.StaticResponse where

import           Data.Aeson (ToJSON)
import           Control.Monad.Reader (reader, runReader)
import qualified Data.Map.Lazy as Map (fromList, lookup)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)

import           Web.StaticAPI.Internal.Types

lookUpPathSegment :: Text -> Environment -> Text
lookUpPathSegment name Environment { namedPathSegments = nps } =
    fromMaybe
        (error ("StaticAPI: Couldn't find variable (" ++ unpack name ++ ") in environment."))
        (Map.lookup name nps)

getPathSegment :: Text -> StaticResponse Text
getPathSegment name = StaticResponse (reader (lookUpPathSegment name))

pathSegments :: StaticResponse [Text]
pathSegments = StaticResponse (reader allPathSegments)

runStaticResponse :: ToJSON a => StaticResponse a -> Environment -> a
runStaticResponse = runReader . runSR
