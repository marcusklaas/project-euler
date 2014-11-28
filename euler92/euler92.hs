import Data.Array
import Data.Char

squareDigits :: Int -> Int
squareDigits = sum . (map ((^2) . digitToInt)) . show

isEightyNiner :: Int -> Bool
isEightyNiner 89 = True
isEightyNiner 1  = False
isEightyNiner n  = eightyNiners ! squareDigits n

eightyNiners :: Array Int Bool
eightyNiners = array (1, 567) [(n, isEightyNiner n) | n <- [1..567]]

main = print . length $ filter isEightyNiner [1..(10^7 - 1)]
