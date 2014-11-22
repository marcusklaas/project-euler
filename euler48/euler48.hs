main = print $ total `mod` 10^10
  where total = sum $ map (\x -> x^x `mod` 10^10) [1..1000]
