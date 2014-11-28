compareDigitsBy :: (Char -> Char -> Bool) -> Int -> Bool
compareDigitsBy f n = and $ zipWith f digits $ tail digits
    where digits = show n
    
isIncreasing :: Int -> Bool
isIncreasing = compareDigitsBy (<=)

isDecreasing :: Int -> Bool
isDecreasing = compareDigitsBy (>=)

isBouncy :: Int -> Bool
isBouncy n = not (isIncreasing n || isDecreasing n)

getFirst :: Int -> Int -> Int -> Int
getFirst multiplier bouncy nonBouncy
    | done             = current - 1
    | isBouncy current = getFirst multiplier (bouncy + 1) nonBouncy
    | otherwise        = getFirst multiplier bouncy       (nonBouncy + 1)
        where done = multiplier * nonBouncy == bouncy
              current = bouncy + nonBouncy + 1

main = print $ getFirst 99 0 100
