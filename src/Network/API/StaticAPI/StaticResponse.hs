module Network.API.StaticAPI.StaticResponse
    ( StaticResponseT
    , StaticResponse
    , ToStaticResponse (..)
    , pathSegments
    , getPathSegment
    , runStaticResponseT
    , runStaticResponse
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (Value, encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Maybe (isJust)
import           Data.Text (Text, unpack)

import           Network.API.StaticAPI.Internal.StaticResponse

type StaticResponseT m a = ReaderT URIParts m a

type StaticResponse a = StaticResponseT IO a

pathSegments :: Monad m => StaticResponseT m [Text]
pathSegments = fmap snd <$> ask

getPathSegment :: Monad m => Text -> StaticResponseT m Text
getPathSegment name = do
    ps <- ask
    let namedSegments = filter (isJust . fst) ps
    case filter (\(Just n, _) -> n == name) namedSegments of
        [(_, value)]    ->
            return value
        _               ->
            error (
                "StaticAPI: Couldn't find variable (" ++
                unpack name ++
                ") in environment or it's duplicated."
            )

runStaticResponseT :: (Monad m, ToStaticResponse a) => StaticResponseT m a -> URIParts -> m a
runStaticResponseT = runReaderT

runStaticResponse :: (MonadIO m, ToStaticResponse a) => StaticResponse a -> URIParts -> m a
runStaticResponse e = liftIO . runStaticResponseT e

class ToStaticResponse a where
    toStaticResponse :: a -> ByteString

instance ToStaticResponse Value where
    toStaticResponse = encode
