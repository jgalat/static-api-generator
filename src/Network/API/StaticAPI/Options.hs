module Network.API.StaticAPI.Options
    ( Options (..)
    ) where

import           Data.Default.Class (Default, def)
import           System.FilePath (FilePath)

newtype Options = Options { outputPath :: FilePath }

instance Default Options where
    def = Options "public"