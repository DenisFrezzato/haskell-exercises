maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) = f a
maybe b _ (Nothing) = b

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = True

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe [head] = Just head

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ma acc -> case ma of
    (Just a) -> a:acc
    Nothing -> acc) []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f as = catMaybes $ map f as
