import Data.Char

main = print . product $ map (digitToInt . (list !!) . (10^)) [0..6]
    where list = concatMap show [0..]
    
-- this version looks way cool but it's actually a lot slower and less reusable than the original
