import Data.Char

main = print . product $ map (digitToInt . (list 1 !!) . (\n -> 10^n - 1)) [0..6]
    where list n = show n ++ list (n + 1)
    
-- this version looks way cooler but it's actually a lot slower and less reusable than the original
