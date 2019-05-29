module Network.API.StaticAPI.StaticAPI
    ( StaticAPI
    , StaticAPIT
    , route
    , root
    , prefix
    , staticAPI
    , staticAPIOpts
    ) where

import           Data.Text (empty)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell, listen, pass)
import qualified Data.ByteString.Lazy as BL (writeFile)
import           Data.Default.Class (def)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (joinPath)

import           Network.API.StaticAPI.Options
import           Network.API.StaticAPI.Path
import           Network.API.StaticAPI.StaticResponse

import           Network.API.StaticAPI.Internal.Endpoint
import           Network.API.StaticAPI.Internal.Route
import           Network.API.StaticAPI.Internal.StaticResponse

type StaticAPIT m a = WriterT [Route m] m a

type StaticAPI a = StaticAPIT IO a

route :: (Monad m, ToPath a, ToStaticResponse b) => a -> StaticResponseT m b -> StaticAPIT m ()
route p sr = tell [Route (toPath p) sr]

root :: (Monad m, ToStaticResponse b) => StaticResponseT m b -> StaticAPIT m ()
root = route empty

prefix :: (Monad m, ToPath a) => a -> StaticAPIT m b -> StaticAPIT m b
prefix r sapi = pass $ do
    (a, _) <- listen sapi
    return (a, map (\(Route p sr) -> Route (toPath r ./ p) sr))

runStaticAPIT :: Monad m => StaticAPIT m a -> m (a, [Route m])
runStaticAPIT = runWriterT

execStaticAPIT :: Monad m => StaticAPIT m a -> m [Route m]
execStaticAPIT = execWriterT

runStaticAPI :: MonadIO m => StaticAPI a -> m (a, [Route IO])
runStaticAPI = liftIO . runStaticAPIT

execStaticAPI :: MonadIO m => StaticAPI a -> m [Route IO]
execStaticAPI = liftIO . execStaticAPIT

staticAPI :: MonadIO m => StaticAPI a -> m ()
staticAPI sapi = staticAPIOpts sapi def

staticAPIOpts :: MonadIO m => StaticAPI a -> Options -> m ()
staticAPIOpts sapi options = do
    routes <- execStaticAPI sapi
    endpoints <- liftIO (mapM toEndpointSequence routes)
    mapM_ (encodeEndpoints options) endpoints

encodeEndpoints :: MonadIO m => Options -> [Endpoint] -> m ()
encodeEndpoints options = mapM_ (liftIO . encodeEndpoint)
    where
        encodeEndpoint :: Endpoint -> IO ()
        encodeEndpoint (Endpoint path response) =
            let rootPath   = outputPath options
                folderPath = joinPath [rootPath, path]
                filePath   = joinPath [folderPath, "index.html"]
            in do
                createDirectoryIfMissing True folderPath
                BL.writeFile filePath (toStaticResponse response)
