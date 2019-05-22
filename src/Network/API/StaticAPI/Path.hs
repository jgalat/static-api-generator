{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Network.API.StaticAPI.Path
    ( Path (..)
    , ToPath (..)
    , (./)
    , (.>)
    ) where

import           Data.Text (Text)

import           Network.API.StaticAPI.Internal.PathSegment

infixl 9 .>
(.>) :: Text -> [Text] -> PathSegment
(.>) name = pathSegment (Just name)

newtype Path = Path [PathSegment]

instance Semigroup Path where
    (<>) (Path ps1) (Path ps2) = Path (ps1 <> ps2)

instance Monoid Path where
    mempty = Path []
    mconcat pss = Path (mconcat (fmap (\(Path ps) -> ps) pss))

class ToPath a where
    toPath :: a -> Path

infixl 8 ./
(./) :: (ToPath a, ToPath b) => a -> b -> Path
(./) a b = toPath a <> toPath b

instance ToPath Text where
    toPath t = Path [pathSegment Nothing [t]]

instance ToPath PathSegment where
    toPath ps = Path [ps]

instance ToPath Path where
    toPath = id

instance ToPath a => ToPath [a] where
    toPath tps =
        let paths = fmap toPath tps
        in Path [pathSegment Nothing (mconcat (fmap go paths))]
        where
            go (Path [ps]) = snd ps