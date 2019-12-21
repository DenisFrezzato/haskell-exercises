{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (($))
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad

newtype RWS r w s a = RWS { runRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
    fmap f (RWS x) = RWS (\r s -> 
        let (a, _, w) = x r s
        in (f a, s, w))

instance Monoid w => Applicative (RWS r w s) where
    pure x = RWS (\r s -> (x, s, mempty))
    RWS fab <*> RWS fa = RWS (\r s ->
        let (b, _, wb) = fab r s
            (a, _, wa) = fa r s
        in (b a, s, wb <> wa))

instance Monoid w => Monad (RWS r w s) where
    return x = RWS (\r s -> (x, s, mempty))
    RWS x >>= k = RWS (\r s ->
        let (a, _, wa) = x r s
            (b, _, wb) = runRWS (k a) r s
        in (b, s, wa <> wb))
