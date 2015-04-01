takeWhileMore :: (a -> Bool) -> [a] -> [a]
takeWhileMore _ []     = []
takeWhileMore p (x:xs)
    | p x       = x : takeWhileMore p xs
    | otherwise = [x]

rectangles :: Integer -> Integer -> Integer
rectangles n m = quot ((m * m + m) * (n * n + n)) 4

rectanglesWithRows :: Integer -> Integer -> [Integer]
rectanglesWithRows limit rowCount = takeWhileMore (< limit) $ map (rectangles rowCount) [1..rowCount]

-- returns (min dist, #cols)
rowBestApproximation :: Integer -> [Integer] -> (Integer, Int)
rowBestApproximation limit xs = minimum $ zip (map (\n -> abs (n - limit)) xs) [1..(length xs)]

-- returns ((min dist, #cols), #rows)
bestApproximation :: [(Integer, Int)] -> ((Integer, Int), Int)
bestApproximation xs = minimum $ zip xs [2..(length xs)]

getArea :: ((Integer, Int), Int) -> Int
getArea ((_, cols), rows) = rows * cols

rowList :: Integer -> [[Integer]]
rowList limit = takeWhileMore ((>1) . length) $ map (rectanglesWithRows limit) [2..]

best :: Integer -> ((Integer, Int), Int)
best limit = bestApproximation $ map (rowBestApproximation limit) $ rowList limit

main = print . getArea $ best 2000000
