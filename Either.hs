{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), Bool(..))
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative
import Control.Monad

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either fac _ (Left a) = fac a
either _ fbc (Right b) = fbc b

lefts :: [Either a b] -> [a]
lefts = foldr (\e acc -> case e of
    (Left a) -> a:acc
    (Right _) -> acc) []

rights :: [Either a b] -> [b]
rights = foldr (\e acc -> case e of
    (Right b) -> b:acc
    (Left _) -> acc) []

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a (Right _) = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b (Left _) = b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (\e (as, bs) -> case e of
    (Left a) -> (a:as, bs)
    (Right b) -> (as, b:bs)) ([], []) 

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

instance Monad (Either a) where
    return = Right
    Right b >>= k = k b
    Left a >>= _ = Left a
