double x = x+x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

average' ns = div (sum ns) (length ns)


n = a `div` length xs
    where
       a = 10
       xs = [1,2,3,4,5]

{- our "menu"
 head, tail, !!, take, drop, length, sum, product, ++, reverse
-}

--Q3
last' xs = head (drop (length xs -1 ) xs )

last'' xs = head $ drop (length xs -1 ) xs 

last''' xs = xs !! (length xs -1)

last'''' xs = head (reverse xs)

last''''' [x] = x
last''''' (x:xs) = last''''' xs

--Q4


--Q4 yet another way


--Q4 alt (with recursion)


--Q5 - init
init' xs = reverse( drop 1 (reverse xs) )

--q5 - alt
init'' xs = take (length xs -1) xs

--q5 - recursion
init''' [x] = [] 
init''' (x:xs) = [x] ++ init''' xs

--q5 - recursion - cons
init'''' [x] = [] 
init'''' (x:xs) = x: init'''' xs

