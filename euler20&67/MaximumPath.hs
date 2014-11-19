module MaximumPath (getMaxPath) where

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

consecutiveTuples :: [a] -> [(a, a)]
consecutiveTuples list
    | length list < 2 = []
    | otherwise       = (tuplify2 $ take 2 list):(consecutiveTuples $ drop 1 list)

updateRow :: [Int] -> [Int] -> [Int]
updateRow current next = zipWith (+) (map (\(x, y) -> max x y) $ consecutiveTuples current) next

getMaxPath :: [Int] -> [[Int]] -> Int
getMaxPath [x] [] = x
getMaxPath [] (head:rest) = getMaxPath head rest
getMaxPath current (head:rest) = getMaxPath (updateRow current head) rest
