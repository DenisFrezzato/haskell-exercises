{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), (.), snd)
import Data.Functor
import Control.Applicative

newtype State s a = State (s -> (s, a))

instance Functor (State s) where
    fmap f (State a) = State (\s -> fmap f $ a s)

instance Applicative (State s) where
    pure x = State (\s -> (s, x))
    State fs <*> State a = State (\s -> fmap (snd $ fs s) $ a s)
