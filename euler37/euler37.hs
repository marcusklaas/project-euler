import Data.Numbers.Primes

truncatable :: Integer -> Bool
truncatable n = (n > 9) && (truncatable' init n) && (truncatable' tail n)
    where truncatable' chop n
              | n < 10    = isPrime n
              | otherwise = isPrime n && (truncatable' chop . read . chop $ show n)  

main = print . sum . take 11 $ filter truncatable [0..]
