and' :: [Bool] -> Bool
--and' [] = True
--and' (False:_) = False
--and' (True:xs) = and' xs

and' [] =True
and' (bool:bools) = bool && and' bools

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss 

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
--replicate' n x = [x] ++ replicate' (n-1) x
replicate' n x = x : replicate' (n-1) x

ind :: [a] -> Int -> a
ind (x:_) 0 = x
ind (_:xs) i = ind xs (i-1) 

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) 
   | x==y = True
   | otherwise = elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
   | x <= y    = x : merge xs (y:ys)
   | otherwise = y : merge (x:xs) ys

msort [] = []
msort [x] = [x]
msort xs = merge (msort ls) (msort rs)
   where
      halfn = length xs `div` 2
      ls = take halfn xs
      rs = drop halfn xs

none _ [] = True
none p xs = (not.(any p) ) xs

