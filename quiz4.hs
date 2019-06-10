whatzygous :: Eq a => a->a -> String
whatzygous a b = if a==b then "Homozygous" else "Heterozygous" 


tooHtooCorJR:: Int -> String
tooHtooCorJR t 
  | t >= 160 = "2 Hot"
  | t < 95   = "*=("
  | otherwise= "=)"


question3 = [('a',"acid"),('b',"base")]


cube x = x*x*x


add3 = \x y z -> x+y+z



first10NonEven5Mults = [ x | x<-[1..100], x `mod` 5 == 0, x `mod` 2 /= 0]
first10NonEven5Mults' = [ 5*x | x<-[1..10], (not.even) 5*x]