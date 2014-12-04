import Data.Numbers.Primes

goldbachNumber :: Int -> Bool
goldbachNumber n = any isPrime [n - 2 * x * x | x <- [0..root]]
    where root = ceiling . sqrt $ fromIntegral n

main = print . head $ filter (not . goldbachNumber) [1 + 2 * i | i <- [1..]]
