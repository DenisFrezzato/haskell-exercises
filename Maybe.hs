{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data Maybe a = Just a | Nothing

instance Foldable Maybe where
    foldMap f (Just a) = f a
    foldMap _ Nothing = mempty

instance Functor Maybe where
    fmap f (Just a) = Just $ f a
    fmap _ Nothing = Nothing

instance Applicative Maybe where
    pure = Just
    Just f <*> x = fmap f x
    Nothing <*> _ = Nothing

instance Traversable Maybe where
    traverse f (Just a) = fmap Just $ f a
    traverse _ Nothing = pure Nothing


