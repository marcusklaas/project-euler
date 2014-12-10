import Data.List
import Data.Digits
import Data.Array

data Colour = Red | Green | Blue deriving (Show, Eq)
type Row = [Colour]

otherColours :: Colour -> [Colour]
otherColours c = distinctColours c c

distinctColours :: Colour -> Colour -> [Colour]
distinctColours a b = delete a $ delete b [Red, Green, Blue]
    
takeEvenIndex :: [a] -> [a]
takeEvenIndex (x:y:xs) = x:takeEvenIndex xs
takeEvenIndex xs       = xs

finishHelper :: Row -> [[Colour]]
finishHelper (x:y:xs) = [x] : distinctColours x y : finishHelper (y:xs)
finishHelper [x]      = [x] : [otherColours x]

finishRow :: [Colour] -> [[Colour]]
finishRow (x:xs) = sequence $ otherColours x : finishHelper (x:xs)

getStubs :: [Colour] -> [[Colour]]
getStubs row = sequence $ fmap otherColours $ takeEvenIndex row

viableContinuations :: [Colour] -> [[Colour]]
viableContinuations row = concatMap finishRow $ getStubs row

rowContinuations :: [Colour] -> Int
rowContinuations row
    | length row == 13 = length $ viableContinuations row
    | otherwise        = sum $ map ((cache !) . encodeRow) $ viableContinuations row
    
cache :: Array Int Int
cache = array (1, 3^13) [(n, (rowContinuations . decodeRow) n) | n <- [1..3^13]]

main = print $ 3 * (rowContinuations [Red])

-- encoding and decoding of row
encodeColor :: Colour -> Int
encodeColor Red = 0
encodeColor Blue = 1
encodeColor Green = 2

decodeColor :: Int -> Colour
decodeColor 0 = Red
decodeColor 1 = Blue
decodeColor 2 = Green

encodeRow :: Row -> Int
encodeRow row = unDigits 3 (1:tail (map (\n -> (n + 3 - head intRow) `mod` 3 ) intRow))
    where intRow = map encodeColor row

decodeRow :: Int -> Row
decodeRow code = map decodeColor $ 0:(tail $ digits 3 code)
