-- ordered lists, difference and union
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

primesB = (2:) . minus [3..] . foldr (\p r-> p*p : union [p*p+p, p*p+2*p..] r) []
           $ primesB

primesBelow2Mil = takeWhile (<2000000) primesB

main = print (sum primesBelow2Mil)
