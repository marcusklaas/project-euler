import Data.Numbers.Primes

main = print . length . (filter isPrime) . takeWhile (< 10^6) $ map (\x -> x^3 - (x-1)^3) [1..]
