either :: (a -> c) -> (b -> c) -> Either a b -> c
either fac _ (Left a) = fac a
either _ fbc (Right b) = fbc b

lefts :: [Either a b] -> [a]
lefts = foldr (\e acc -> case e of
    (Left a) -> a:acc
    (Right _) -> acc) []

rights :: [Either a b] -> [b]
rights = foldr (\e acc -> case e of
    (Right b) -> b:acc
    (Left _) -> acc) []

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a (Right _) = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b (Left _) = b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (\e (as, bs) -> case e of
    (Left a) -> (a:as, bs)
    (Right b) -> (as, b:bs)) ([], [])

