identity :: a -> a
identity a = a

tuple :: a -> (a, a)
tuple a = (a, a)

map :: (a -> b) -> a -> b
map f a = f a

tupleMap :: (a -> b -> c) -> (a, b) -> c
tupleMap f (a, b) = f a b

