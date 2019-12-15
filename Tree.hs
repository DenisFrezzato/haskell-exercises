{-# LANGUAGE NoImplicitPrelude #-}

import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data Tree a = Leaf | Tree a (Tree a) (Tree a)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Tree n t' t'') = f n <> foldMap f t' <> foldMap f t''

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


