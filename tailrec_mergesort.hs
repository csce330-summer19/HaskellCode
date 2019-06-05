
t_ms xs = t_ms_helper 1 (length xs ) xs

t_ms_helper n len xs 
    | n >= len = xs
    | otherwise = t_ms_helper (2*n) len tms
    where tms = takeMerge n xs

takeMerge _ [] = []
takeMerge n xs =   merged ++ ( takeMerge n $ drop (2*n) xs )
    where
        first = take n xs
        second =  take n ( drop n xs )
        merged = merge first second 

merge [] [] = []
merge xs [] = xs
merge [] ys = ys        
merge xs ys
        | head xs <= head ys = head xs : merge (tail xs) ys
        | otherwise          = head ys : merge xs (tail ys)