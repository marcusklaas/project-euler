moreFibo :: (Num a) => [a] -> [a] 
moreFibo list
    | length list < 1 = error "Fibonacci list too short"
	| otherwise       = (sum (take 2 list)):list

smallFibo :: [Int] -> Int -> [Int]
smallFibo list limit
    | head list > limit = drop 1 list
    | otherwise         = smallFibo (moreFibo list) limit


allSmallFibos = smallFibo [2,1] 4000000
evenSmallFibos = filter even allSmallFibos


main = print (sum evenSmallFibos)
