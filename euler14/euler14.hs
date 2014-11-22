import Data.Array
import Data.List

cacheSize :: Int
cacheSize = 50000

cache :: Array Int Int
cache = array (1, cacheSize) [(i, collatz i) | i <- [1..cacheSize]]

cachedCollatz :: Int -> Int
cachedCollatz n
  | n <= cacheSize = cache ! n
  | otherwise      = collatz n

collatz :: Int -> Int
collatz 1 = 1
collatz n
  | even n    = cachedCollatz (n `quot` 2) + 1
  | otherwise = cachedCollatz (3 * n + 1) + 1
  
getMaximiser :: (Ord y) => (x -> y) -> [x] -> x
getMaximiser f [] = error "getMaximiser: empty list"
getMaximiser f list = snd $ maximumBy cmpFst $ zip (map f list) list
  where cmpFst (a,_) (b,_) = compare a b

main = print $ getMaximiser collatz [1..999999]
