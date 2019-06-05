-- higher order functions examples... will do many
-- technically, a higher order function is a function that 
-- takes OR returns a function
-- but most functions that take 2+ args return functions 
-- as intermediate values (currying) so we 
-- reserve "higher order" in Haskell for functions
-- that take functions as arguments

-- working recursively

sum' [] = 0
sum' (x:xs) = x + sum' xs

-- this above pattern is getting repetitive...

-- using a Higher order function

sum'' xs = foldr (+) 0 xs

--or even just, (sum IS a _function_)
sum''' :: Num a => [a] -> a
sum''' = foldl (+) 0

