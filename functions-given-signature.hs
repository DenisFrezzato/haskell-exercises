identity :: a -> a
identity a = a

tuple :: a -> (a, a)
tuple a = (a, a)

map' :: (a -> b) -> a -> b
map' f a = f a

tupleMap :: (a -> b -> c) -> (a, b) -> c
tupleMap f (a, b) = f a b

exercise9 :: Applicative f => [(f a, b)] -> f [(a, b)]
exercise9 = exercise10

exercise10 :: (Traversable t, Applicative f) => t (f a, b) -> f (t (a, b))
exercise10 = traverse (\(fa, b) -> fmap (\a -> (a, b)) fa)
