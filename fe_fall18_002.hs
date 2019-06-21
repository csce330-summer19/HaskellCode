
--q13
nthMultiple m n = last [ m*i| i<-[1..n] ]

nthMultiple' m n = sum  [ m | i<-[1..n] ]

allMultiples m = [ m*i| i<-[1..] ]

--q14
bwAnd :: String -> String -> String
bwAnd "" "" = ""
bwAnd (a:as) (b:bs) 
    | a=='1' && b=='1' = '1' : bwAnd as bs
    | otherwise = '0' : bwAnd as bs

--q15
dupSepBy1 [] = False
dupSepBy1 [_] = False
dupSepBy1 [_,_] = False
dupSepBy1 (x:xs)
    | x == xs !! 1 = True
    | otherwise = dupSepBy1 xs

--q16
type Student = (String,String,String)

--q17
type NumDate = (Int,Int,Int)

--q18
isWaldoThere :: [String] -> Bool
isWaldoThere ss = elem "Waldo" ss

isWaldoThere' :: [String] -> Bool
isWaldoThere' ss = any (==True) $ map (\s->s=="Waldo") ss



isWaldoThere'' :: [String] -> Bool
isWaldoThere'' ss = foldr (\ t ts -> t=="Waldo"|| ts) False ss

isWaldoThere''' :: [String] -> Bool
isWaldoThere''' ss = length filtered > 0
    where 
        filtered = filter (\s->s=="Waldo") ss


--q19 -- with a recursion
bitFlip [] = []
bitFlip (b:bs) = (if b=='0' then '1' else '0'):bitFlip bs