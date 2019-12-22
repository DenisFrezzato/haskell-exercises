{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), Bool(..), error)
import Data.List
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative
import Control.Monad

data Maybe a = Just a | Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) = f a
maybe b _ Nothing = b

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = True

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [head] = Just head

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ma acc -> case ma of
    (Just a) -> a:acc
    Nothing -> acc) []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f as = catMaybes $ map f as

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
    traverse f (Just a) = Just <$> f a
    traverse _ Nothing = pure Nothing

instance Monad Maybe where
    return = Just
    Just x >>= k = k x
    Nothing >>= _ = Nothing
