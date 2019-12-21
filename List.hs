{-# LANGUAGE NoImplicitPrelude #-}

import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data List a = Nil | Cons a (List a)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a as) = f a <> foldMap f as

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure x = Cons x Nil
    Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)
    Cons _ _ <*> Nil = Nil
    Nil <*> _ = Nil

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a as) = fmap Cons (f a) <*> traverse f as
