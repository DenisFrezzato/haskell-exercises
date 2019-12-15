{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Foldable
import Data.Traversable
import Data.Functor

newtype Identity a = Identity a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Traversable Identity where
    traverse f (Identity a) = fmap Identity $ f a
