{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), (.), snd)
import Data.Functor
import Control.Applicative
import Control.Monad

newtype State s a = State (s -> (s, a))

instance Functor (State s) where
    fmap f (State a) = State $ fmap f . a

instance Applicative (State s) where
    pure x = State (\s -> (s, x))
    State fs <*> State a = State (\s -> snd (fs s) <$> a s)

instance Monad (State s) where
    return x = State (\s -> (s, x))
    State a >>= k = State (\s -> 
        let (State b) = k $ snd $ a s
        in b s)
