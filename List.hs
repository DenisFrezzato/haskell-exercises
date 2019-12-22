{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), Show)
import Data.Foldable
import Data.Semigroup
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative
import Control.Monad

data List a = Nil | Cons a (List a) deriving (Show)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a as) = f a <> foldMap f as

instance Semigroup (List a) where 
    Nil <> x = x
    x <> Nil = x
    Cons a as <> x = Cons a $ as <> x

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (f <$> as)

instance Applicative List where
    pure x = Cons x Nil
    Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)
    _ <*> Nil = Nil
    Nil <*> _ = Nil

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Monad List where
    return x = Cons x Nil
    Nil >>= _ = Nil
    Cons a as >>= k = case k a of
        Cons y ys -> Cons y $ ys <> (as >>= k)
        Nil -> as >>= k
