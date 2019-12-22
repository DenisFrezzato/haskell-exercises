{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Foldable
import Data.Traversable
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Identity a = Identity a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> x = fmap f x

instance Monad Identity where
    return = Identity
    Identity a >>= k = k a
