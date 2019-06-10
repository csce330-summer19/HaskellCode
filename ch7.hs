mf f p xs = map f (filter p xs )

mf' f p = (map f ).(filter p)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\a as-> f a: as ) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr (\a as -> if p a then a:as else as ) [] xs