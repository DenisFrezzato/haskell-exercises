{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($), (.), const, fst, snd)
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    Writer (f, wf) <*> Writer (a, wa) = Writer (f a, wf <> wa)

instance Monoid w => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (a, w) >>= k = Writer (fst z, w <> snd z) where
        z = runWriter $ k a
