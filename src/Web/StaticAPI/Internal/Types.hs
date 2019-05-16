{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.Internal.Types where

import           Data.Aeson hiding (Options)
import           Data.Default.Class (Default, def)
import           Data.Map.Lazy (Map)
import           Data.Text (Text)
import           Control.Monad.Reader (Reader)
import           Control.Monad.Writer (Writer)
import           Control.Monad.Fail (MonadFail (..))
import           System.FilePath (FilePath)

newtype Options = Options { outputPath :: FilePath }

instance Default Options where
    def = Options "public"

data PathSegment = PathSegment
    { name      :: Maybe Text
    , values    :: [Text]
    }

infixl 9 .>
(.>) :: Text -> [Text] -> PathSegment
(.>) name = PathSegment (Just name)

newtype Path = Path [PathSegment]

class PathComponent a where
    toPath :: a -> Path

infixl 8 ./
(./) :: (PathComponent a, PathComponent b) => a -> b -> Path
(./) pc1 pc2 =
    case (toPath pc1, toPath pc2) of
        (Path pss1, Path pss2) -> Path (pss1 ++ pss2)

instance PathComponent Text where
    toPath t = Path [PathSegment Nothing [t]]

instance PathComponent PathSegment where
    toPath ps = Path [ps]

instance PathComponent Path where
    toPath = id

instance (PathComponent a) => PathComponent [a] where
    toPath pcs =
        let paths = map toPath pcs
        in Path [PathSegment Nothing (foldr (\p pss -> vals p ++ pss) [] paths)]
        where
            vals (Path [PathSegment Nothing vs]) = vs
            vals _                               =
                error "StaticAPI: Only [Text] is allowed to be used with (./)"

data Environment = Environment
    { allPathSegments   :: [Text]
    , namedPathSegments :: Map Text Text
    }

newtype StaticResponse a = StaticResponse { runSR :: Reader Environment a }
                         deriving (Functor, Applicative, Monad)

instance MonadFail StaticResponse where
    fail _ = error "StaticAPI: Left hand side doesn't match"

data Route = forall a . (ToJSON a) => Route Path (StaticResponse a)

newtype StaticAPIM a  = StaticAPIM { runSAPI :: Writer [Route] a }
                      deriving (Functor, Applicative, Monad)

type StaticAPI = StaticAPIM ()

data Endpoint = forall a . (ToJSON a) => Endpoint FilePath a
