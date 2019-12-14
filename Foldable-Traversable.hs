{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data List a = Nil | Cons a (List a)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a as) = f a `mappend` foldMap f as

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
    traverse f (Cons a as) = fmap Cons (f a) <*> (traverse f as)

newtype Identity a = Identity a

instance Foldable Identity where
    foldMap f (Identity a) = f a

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Traversable Identity where
    traverse f (Identity a) = fmap Identity $ f a

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

data Tree a = Leaf | Tree a (Tree a) (Tree a)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Tree n t' t'') = f n `mappend` foldMap f t' `mappend` foldMap f t''

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Tree a t' t'') = Tree (f a) (fmap f t') (fmap f t'')

instance Applicative Tree where
    pure x = Tree x Leaf Leaf
    Tree f ft' ft'' <*> (Tree a t' t'') = Tree (f a) (ft' <*> t') (ft'' <*> t'')
    _ <*> Leaf = Leaf
    Leaf <*> _ = Leaf

instance Traversable Tree where
    traverse _ Leaf = pure Leaf
    traverse f (Tree a t' t'') = fmap Tree (f a) <*> traverse f t' <*> traverse f t'' 

data Pair a = Pair a a

instance Foldable Pair where
    foldMap f (Pair x y) = f x `mappend` f y

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    Pair f g <*> Pair x y = Pair (f x) (g y)

instance Traversable Pair where
    traverse f (Pair x y) = fmap Pair (f x) <*> (f y)
