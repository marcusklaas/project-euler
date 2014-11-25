import Data.Numbers.Primes
import Data.List

-- make sure to compile with -O flag. this results in a ~8x speedup

subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = [x:s | s <- subs] ++ subs
  where subs = subsets xs

-- candidates for the first subset
candidates :: String -> [String]
candidates (x:xs) = map (x:) (subsets xs)

-- the number of permutations of candidate which are prime
candidatePrimes :: String -> Int
candidatePrimes = length . (filter (isPrime . read)) . permutations

-- the number of pandigital sets with given candidate
setsWithCandidate :: String -> String -> Int
setsWithCandidate xs can
    | primes > 0 = primes * (pandigitSets $ xs \\ can)
    | otherwise  = 0
    where primes = candidatePrimes can

pandigitSets :: String -> Int
pandigitSets [] = 1
pandigitSets xs = sum . map (setsWithCandidate xs) . candidates $ xs

main = print $ pandigitSets "123456789"
