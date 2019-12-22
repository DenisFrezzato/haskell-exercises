{-# LANGUAGE NoImplicitPrelude #-}

import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data Tree a = Leaf | Tree a (Tree a) (Tree a)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Tree a tl tr) = f a <> foldMap f tl <> foldMap f tr

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap f (Tree a tl tr) = Tree (f a) (f <$> tl) (f <$> tr)

instance Applicative Tree where
    pure x = Tree x Leaf Leaf
    Tree f ftl ftr <*> (Tree a tl tr) = Tree (f a) (ftl <*> tl) (ftr <*> tr)
    _ <*> Leaf = Leaf
    Leaf <*> _ = Leaf

instance Traversable Tree where
    traverse _ Leaf = pure Leaf
    traverse f (Tree a tl tr) = Tree <$> f a <*> traverse f tl <*> traverse f tr 


