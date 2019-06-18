import Data.List

data HTree = CNode Double Char
            | INode Double HTree HTree deriving Show


freq :: HTree -> Double
freq (CNode f _) = f
freq (INode f _ _ ) = f

merge :: HTree -> HTree -> HTree
merge x y = INode (freq x + freq y) x y

{-
--mergeAllTrees' :: [HTree]  -> HTree 
--mergeAllTrees' [t] = t
--mergeAllTrees' ts = mergeAllTrees' (t:rest)
--    where
--        (f:s:rest) = sortOn freq ts
--        t = merge f s
-}
        
mergeAllTrees :: [HTree] -> HTree
mergeAllTrees [t] = t
mergeAllTrees ts = mergeAllTrees (t:rest)
    where
        compf = (\t1 t2 -> freq t1 <= freq t2)
        mini1 = mini compf ts
        tsm1 = removei mini1 ts
        mini2 = mini compf tsm1
        rest = removei mini2 tsm1
        t = merge (ts!!mini1) (tsm1!!mini2)
        

mini :: (a->a->Bool)-> [a] -> Int
mini _ [x] = 0
mini compf (x:xs) = if compf x (xs!!minixs) then 0 else 1+ minixs 
    where
        minixs = mini compf xs 




--list comprehension
removei :: Int -> [a] -> [a]
removei index xs = [ x | (x,i)<-zip xs [0..], i /= index ]


--recursion
removei' :: Int -> [a] -> [a]
removei' 0 (x:xs) = xs
removei' i (x:xs) = x: removei' (i-1) xs 

--other way?
removei'' :: Int-> [a] ->[a]
removei'' i xs = take i xs ++ drop (i+1) xs

buildHTree :: [(Char,Double)] -> HTree 
buildHTree charfreqs = mergeAllTrees cnodes
        where
            cnodes = map (\ (ch,fr)->CNode fr ch) charfreqs


decodeTree :: String -> HTree -> [(Char,String)]
decodeTree pre (CNode _ c) = [(c,pre)]
decodeTree pre (INode _ left right) = lcodes ++ rcodes
    where
        lcodes = decodeTree (pre++"0") left
        rcodes = decodeTree (pre++"1") right

--minor bug: what if there's only one char? who cares...
getCodes :: [(Char,Double)] -> [(Char,String)]
getCodes cfs = decodeTree "" (buildHTree cfs)

{-
getCodes' :: [(Char,Double)] -> [(Char,String)]
-}

get350Slide38 = [('A',0.35),('B',0.1),('C',0.2),('D',0.2),('_',0.15)] 
        
