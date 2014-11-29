import Data.List

isPandigital :: Int -> Bool
isPandigital = (== "123456789") . sort . show

transform :: Int -> Int
transform n = read $ head $ filter ((>=9) . length) $ scanl1 (++) $ map show $ iterate (+n) n

main = print . maximum $ filter isPandigital $ map transform [1..9999]
