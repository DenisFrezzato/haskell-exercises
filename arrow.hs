import Data.Functor
import Control.Applicative
import Control.Monad

instance Functor ((->) a) where
    fmap f g = g . f

instance Applicative ((->) a) where
    pure = const 
    f <*> g = \x -> f x $ g x

instance Monad ((->) a) where
    return = const
    f >>= k = \x -> k (f x) x
