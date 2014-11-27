import Data.Numbers.Primes
import Data.List

fourDigitPrimes :: [Int]
fourDigitPrimes = filter (>= 1000) $ takeWhile (< 10000) primes

legitGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
legitGroupBy _ [] = []
legitGroupBy f xs = fst part:(legitGroupBy f $ snd part)
    where part = partition (f (head xs)) xs

isAnagram :: Int -> Int -> Bool
isAnagram a b = digits a == digits b
    where digits = sort . show

anagrams :: [[Int]]
anagrams = legitGroupBy isAnagram fourDigitPrimes

lenThreeArit :: [Int] -> [(Int, Int, Int)]
lenThreeArit xs = map (\(x, y) -> (x, y, y + y - x)) pairs
    where pairs = filter hasThird [(x, y) | x <- xs, y <- xs, x < y]
          hasThird (x, y) = (y + y - x) `elem` xs
          
present :: (Int, Int, Int) -> String
present (x0, x1, x2) = show x0 ++ show x1 ++ show x2

main = putStrLn $ present answer
    where answer = head $ filter (/= (1487, 4817, 8147)) answerList
          answerList = concat $ map lenThreeArit anagrams
