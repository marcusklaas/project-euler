memoize :: (Int -> Int -> a) -> [[a]]
memoize f = map (\x -> map (f x) [0..]) [0..]

store :: [[Int]]
store = memoize paths

memoized_paths :: Int -> Int -> Int
memoized_paths x y 
    | x < y     = memoized_paths y x
    | otherwise = store !! x !! y

paths :: Int -> Int -> Int
paths x 0 = 1
paths 0 y = 1
paths x y = memoized_paths x (y - 1) + memoized_paths (x - 1) y

main = print $ paths 20 20
