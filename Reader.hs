{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), (.), const)
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader g) = Reader $ f . g 

instance Applicative (Reader r) where
    pure a = Reader $ const a 
    Reader f <*> Reader a = Reader (\r -> f r $ a r)

instance Monad (Reader r) where
    return a = Reader $ const a
    Reader f >>= k = Reader (\r -> runReader (k $ f r) r)
