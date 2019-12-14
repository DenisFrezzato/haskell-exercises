{-# LANGUAGE NoImplicitPrelude #-}

import Data.Foldable
import Data.Monoid

--data [a] = [] | (a:[a]) -- The standard list type
--
--instance Foldable [a] where
--    foldMap _ [] = mempty
--    foldMap f (a:as) = f a `mappend` foldMap as

newtype Identity a = Identity a

instance Foldable Identity where
    foldMap f (Identity a) = f a

data Maybe a = Just a | Nothing

instance Foldable Maybe where
    foldMap f (Just a) = f a
    foldMap _ Nothing = mempty

data Either a b = Left a | Right b

instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right b) = f b

data Tree a = Leaf | Tree a (Tree a) (Tree a)

instance Foldable Tree where
    foldMap _ Leaf = mempty
    foldMap f (Tree n t' t'') = f n `mappend` foldMap f t' `mappend` foldMap f t''

data Pair a = Pair a a

instance Foldable Pair where
    foldMap f (Pair x y) = f x `mappend` f y
