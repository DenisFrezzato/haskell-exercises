{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data Either a b = Left a | Right b

instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right b) = f b

instance Functor (Either a) where
    fmap f (Right b) = Right $ f b
    fmap _ (Left a) = Left a

instance Applicative (Either a) where
    pure = Right
    Right f <*> x = fmap f x
    Left a <*> _ = Left a

instance Traversable (Either a) where
    traverse _ (Left a) = pure $ Left a
    traverse f (Right b) = fmap Right $ f b


