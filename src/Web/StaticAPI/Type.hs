{-# LANGUAGE ExistentialQuantification #-}
module Web.StaticAPI.Type where

import           Control.Monad
import           Data.Aeson
import           Web.StaticAPI.VarMap

data SubPath  = Constant String
              | Variable String [String]
              deriving (Eq, Show)

type Path = [SubPath]

data Route = forall a . (ToJSON a) => Route Path (VarMap -> a)

data StaticAPIM a = StaticAPIM a [Route]

type StaticAPI = StaticAPIM ()

instance Functor StaticAPIM where
  fmap = undefined

instance Applicative StaticAPIM where
  pure a = StaticAPIM a []
  (<*>)  = ap

instance Monad StaticAPIM where
  return = pure
  StaticAPIM a xs >>= f = case f a of
      StaticAPIM b ys -> StaticAPIM b (xs ++ ys)
