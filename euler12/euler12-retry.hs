countDevisors :: Int -> Int
countDevisors n
    | isPerfectSquare n = 2 * sqCount - 1
    | otherwise         = 2 * sqCount
    where sqCount = length $ filter (\x -> (n `mod` x == 0)) [1..sq]
          sq = floor $ sqrt $ (fromIntegral n)

isPerfectSquare n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n)

getAnswer :: Int -> Int -> Int
getAnswer triangle index
    | countDevisors triangle <= 500 = getAnswer (triangle + index + 1) (index + 1)
    | otherwise                     = triangle

main = print $ getAnswer 1 1
