import Data.List
import Data.Char

allEqual :: (Eq x) => [x] -> Bool
allEqual (x:xs) = all (== x) xs

getSortedDigits :: Int -> [Int]
getSortedDigits = sort . (map digitToInt) . show

multiplesPermuted :: Int -> Bool
multiplesPermuted = allEqual . (map getSortedDigits) . sequence (map (*) [1..6])

main = print . (find multiplesPermuted) $ [1..]
