fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib x = fib (x-2) + fib (x-1)

fib' :: Integer -> Integer
fib' 1 = 0
fib' 2 = 1
fib' x = h 0 1 3 x

h fibm2 fibm1 m n 
   | m == n = fibm2 + fibm1
   | otherwise = h fibm1 (fibm1+fibm2) (m+1) n

l1Norm :: Num a => [a] -> a
l1Norm xs = sum ( map abs xs)

l1Norm' :: Num a => [a] -> a
l1Norm' xs = foldr (+) 0 (map abs xs)

l1Norm'' :: Num a => [a] -> a
l1Norm'' xs = foldr (\x ys -> abs x + ys) 0 xs

l2Norm :: [Float] -> Float
l2Norm xs = sqrt $ foldr (\x ys -> x**2 + ys) 0 xs