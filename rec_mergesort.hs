ms [] = []
ms [x] = [x]
ms xs = merge sys szs
    where
        (ys,zs) = split xs
        sys = ms ys
        szs = ms zs


split xs = splitn (length xs `div` 2) xs

splitn 0 zs = ([],zs)
splitn n (x:xs) = ( (x:ys) , zs)
    where
        (ys,zs) = splitn (n-1) xs

merge [] [] = []
merge xs [] = xs
merge [] ys = ys        
merge xs ys
        | head xs <= head ys = head xs : merge (tail xs) ys
        | otherwise          = head ys : merge xs (tail ys)