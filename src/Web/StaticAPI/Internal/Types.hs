{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.Internal.Types where

import           Data.Aeson hiding (Options)
import           Data.Default.Class (Default, def)
import           Data.Map.Lazy (Map)
import           Control.Monad.Reader (Reader)
import           Control.Monad.Writer (Writer)

data Options = Options  { outputDirectory :: FilePath
                        , outputFile      :: FilePath
                        }

instance Default Options where
    def = Options "output" "index.html"

type Environment = Map String String

newtype StaticResponse a  = StaticResponse { runSR :: Reader Environment a }
                          deriving (Functor, Applicative, Monad)

data SubPath  = Constant String
              | Variable String [String]
              deriving (Eq, Show)

type Path = [SubPath]

data Route = forall a . (ToJSON a) => Route Path (StaticResponse a)

newtype StaticAPIM a  = StaticAPIM { runSAPI :: Writer [Route] a }
                      deriving (Functor, Applicative, Monad)

type StaticAPI = StaticAPIM ()

type RawPath  = String

data EndPoint = forall a . (ToJSON a) => EndPoint FilePath a
