nodups ::Eq a => [a] -> [a]
nodups [] = []
nodups (x:xs) = if elem x xs then nodups xs else x : nodups xs

evenSum :: [(Int,Int)] -> [Int]
evenSum xs = filter (\x-> mod x 2 == 0) (map (\(x,y)->x+y) xs )

evenSum' :: [(Int,Int)] -> [Int]
evenSum' xs = foldr (\(x,y) rest -> if(x+y)`mod`2==0 then (x+y):rest else rest) [] xs