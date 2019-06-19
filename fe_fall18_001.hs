g x y z = x+ y*z

f (x,y) = x == y

q7 = [('1',"3"),('c',"chili")]


mid :: (a,b,c) -> b
mid = (\(_,y,_)->y)

indexEnd :: [a] -> Int -> a
indexEnd = (\xs j -> reverse xs !! j)

indexEnd' :: [a] -> Int -> a
indexEnd' = (\xs j -> xs !! (length xs - 1 -j) )


bitFlip :: String -> String
bitFlip bs= [ flip b | b<-bs]
    where 
        flip b = if b=='0' then '1' else '0'


doubleEverything :: [a] -> [a]
doubleEverything xs = foldr (\p ps->fst p: snd p : ps) [] [ z|z <-zip xs xs ]

doubleEverything' :: [a] -> [a]
doubleEverything' xs = [ x | x<-xs , y<-[0,1] ]
--this one with foldr
doubleEverything'' :: [a] -> [a]
doubleEverything'' xs = foldr (\p ps->fst p: snd p : ps) [] ( zip xs xs )

doubleEverything''' :: [a] -> [a]
doubleEverything''' xs = [ xs !! (di `div` 2) | di <-[0..2*length xs - 1]]

evenSquareds :: [Int] -> [Int]
evenSquareds xs = map (^2) $ filter even xs

evenSquareds' :: [Int] -> [Int]
evenSquareds' [] = []
evenSquareds' (x:xs) 
    | odd x = evenSquareds' xs
    | otherwise = x*x : evenSquareds' xs
{-
addTicks :: String -> String -> String

evenPowersOf3 :: Num a => [a]

-}

