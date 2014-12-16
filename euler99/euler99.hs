import System.IO
import Data.List.Split (splitOn)

tuplify :: [a] -> (a, a)
tuplify [x,y] = (x,y)

toTuples :: String -> [(Integer, Integer)]
toTuples = map (tuplify . map read . splitOn ",") . lines

exponents :: (Integer, Integer) -> Double
exponents (x, y) = log (fromIntegral x) * fromIntegral y

getMax :: String -> Int
getMax s = snd . maximum $ zip (map exponents $ toTuples s) [1..]

main = do
    file <- openFile "p099_base_exp.txt" ReadMode
    input <- hGetContents file
    print $ getMax input
