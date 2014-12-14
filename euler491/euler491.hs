combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
    where combinations' n k' l@(y:ys)
            | k' == 0   = [[]]
            | k' >= n   = [l]
            | null l    = []
            | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

characters :: [Integer]          
characters = [0..9] ++ [0..9]

permutationsCount :: Integer
permutationsCount = product $ [2..9] ++ [2..10]

perms :: [Integer] -> Integer
perms xs = quot total (2^10)
    where total = permutationsCount * (-) 10 (toInteger . length $ filter (== 0) xs)
    
divisible :: [Integer] -> Bool
divisible xs = (90 - 2 * sum xs) `mod` 11 == 0

main = print . sum . map perms . filter divisible $ combinations 10 characters
