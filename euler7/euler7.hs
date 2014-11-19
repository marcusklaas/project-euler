hasDivisor :: Int -> [Int] -> Bool
hasDivisor candidate [] = False
hasDivisor candidate list = candidate `mod` (head list) == 0 || hasDivisor candidate (drop 1 list) 

nextPrime :: [Int] -> Int -> Int
nextPrime list candidate
    | length list < 1           = error "Cannot compute next prime when prime list empty"
    | hasDivisor candidate list = nextPrime list (candidate + 1)
    | otherwise                 = candidate 

generatePrimes :: Int -> [Int]
generatePrimes 0 = []
generatePrimes 1 = [2]
generatePrimes n = x ++ [nextPrime x (last x)]
   where x = generatePrimes (n-1)

main = print (last (generatePrimes 10001))
