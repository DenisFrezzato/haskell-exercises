{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Functor
import Control.Applicative

data Pair a = Pair a a

instance Foldable Pair where
    foldMap f (Pair x y) = f x <> f y

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    Pair f g <*> Pair x y = Pair (f x) (g y)

instance Traversable Pair where
    traverse f (Pair x y) = Pair <$> f x <*> f y
