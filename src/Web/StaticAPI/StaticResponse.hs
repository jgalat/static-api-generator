module Web.StaticAPI.StaticResponse where

import           Data.Aeson (ToJSON)
import           Control.Monad.Reader (reader, runReader)
import qualified Data.Map.Lazy as Map (fromList, lookup)
import           Data.Maybe (fromMaybe)

import           Web.StaticAPI.Internal.Types

fromList :: [(String, String)] -> Environment
fromList = Map.fromList

getVariable :: String -> Environment -> String
getVariable s e =
  fromMaybe
    (error ("StaticAPI: Couldn't find variable (" ++ s ++ ") in environment."))
    (Map.lookup s e)

readVariable :: String -> StaticResponse String
readVariable s = StaticResponse (reader (getVariable s))

getStaticResponse :: ToJSON a => StaticResponse a -> Environment -> a
getStaticResponse = runReader . runSR
