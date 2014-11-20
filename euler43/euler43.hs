import Data.List
import Data.Char

-- this is a painfully naive approach

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = [x:ys | x <- xs, ys <- permute (delete x xs)]

threeDigitCheck :: String -> Int -> Int
threeDigitCheck str i = read $ map (\x -> str !! (i + x - 1)) [0..2]

smallPrimes = [(2, 2), (3, 3), (4, 5), (5, 7), (6, 11), (7, 13), (8, 17)]

satisfiesCheck :: String -> Bool
satisfiesCheck str = all (\(i, p) -> threeDigitCheck str i `mod` p == 0) $ reverse smallPrimes

validPandigitals = filter satisfiesCheck (permutations "0123456789")

main = print $ sum $ map read validPandigitals
