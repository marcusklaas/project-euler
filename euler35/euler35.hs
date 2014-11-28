import Data.Numbers.Primes

rotations :: [a] -> [[a]]
rotations xs = take (length xs) $ iterate rotate xs
    where rotate xs = tail xs ++ [head xs]

isCircularPrime :: String -> Bool
isCircularPrime = (all (isPrime . read)) . rotations

block :: [a] -> Int -> [[a]]
block chars 0   = [[]]
block chars len = [c:set | c <- chars, set <- nextSet]
    where nextSet = block chars (len - 1)

inputSet :: [String]
inputSet = concat $ map (block "1379") [1..6]

main = print $ length (filter isCircularPrime inputSet) + 2
