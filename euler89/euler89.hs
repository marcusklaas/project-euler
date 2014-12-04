import System.IO
import Data.Bimap
import Data.Char

mapping :: Bimap Char Int
mapping = fromList [('M', 1000), ('D', 500), ('C', 100), ('L', 50), ('X', 10), ('V', 5), ('I', 1)]

charValue :: Char -> Maybe Int
charValue c = Data.Bimap.lookup c mapping

negatePrefix :: (Num a, Ord a) => [a] -> [a]
negatePrefix (x:y:xs) = (if x < y then negate x else x):(negatePrefix $ y:xs)
negatePrefix xs       = xs

wordValue :: String -> Maybe Int
wordValue w = fmap (sum . negatePrefix) $ sequence $ fmap charValue w
    --valueList <- sequence $ fmap charValue w
    --return . sum $ negatePrefix valueList

roman :: Int -> String
roman n
    | n == 0           = []
    | n >= 1000        = replicate (quot n 1000) 'M'      ++ roman (mod n 1000)
    | mod digit 5 == 4 = mapping !> one                    : roman (n + one)
    | digit >= 5       = mapping !> (5 * one)              : roman (n - 5 * one)
    | otherwise        = replicate digit (mapping !> one) ++ roman (n - digit * one)
    where order = length . tail $ show n
          digit = digitToInt . head $ show n
          one = 10^order
    
superfluousChars :: String -> Maybe Int
superfluousChars w = fmap (\x -> length w - (length . roman) x) $ wordValue w

main = do
    file <- openFile "p089_roman.txt" ReadMode
    input <- hGetContents file
    print . (fmap sum) . sequence . (fmap superfluousChars) $ lines input
