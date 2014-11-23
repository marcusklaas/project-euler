import Data.List.Ordered
import Data.Char
import Data.List.Split

triangles :: [Int]
triangles = map (\n -> quot (n * (n - 1)) 2) [1..]

isTriangle :: Int -> Bool
isTriangle = has triangles

wordValue :: String -> Int
wordValue = sum . (map (\char -> 1 + ord char - ord 'A'))

isTriangleWord :: String -> Bool
isTriangleWord = isTriangle . wordValue

readWords :: String -> [String]
readWords = (splitOn "\",\"") . (drop 1) . init

main = do
    input <- readFile "p042_words.txt"
    print . length . (filter isTriangleWord) . readWords $ input
